import hmac
import hashlib
from time import time
from urllib.parse import urlencode
import json
import aiohttp


class ErrorResponse(Exception):
    pass


def to_byte(s):
    if isinstance(s, str):
        return bytes(s, 'utf-8')
    elif isinstance(s, bytes):
        return s
    else:
        return bytes(str(s), 'utf-8')


def sign_params(secret, params):
    m = hmac.new(to_byte(secret), digestmod=hashlib.sha256)
    update_json(m, params)

    return m.hexdigest().upper()


def update_json(m, params):
    if isinstance(params, list):
        for param in params:
            update_json(m, param)

    elif isinstance(params, dict):
        params = sorted(params.items(), key=lambda x: x[0])
        for k, v in params:
            m.update(to_byte(k))
            update_json(m, v)

    elif isinstance(params, bool):
        if params:
            m.update(b'true')
        else:
            m.update(b'false')
    else:
        m.update(to_byte(params))


class Gateway(object):
    def __init__(self, host, key, secret=None, makeSecret=None, secure=False):
        self._host = host
        self._key = key
        self._secret = secret
        self._secure = secure
        self._makeSecret = makeSecret

        if makeSecret:
            self._caches = {}


    async def get_headers(self, method='GET', pathname = '', params = {}, secure = True):
        if secure:
            params = params.copy()
            params['key'] = self._key
            params['pathname'] = pathname
            if self._makeSecret:
                secret = await self.get_secret(method, pathname)
                params['timestamp'] = str(secret['timestamp'])
                sign = sign_params(secret['secret'], params)
                return {
                    'X-REQUEST-KEY': self._key,
                    'X-REQUEST-SIGNATURE': sign,
                    'X-REQUEST-TIME': str(secret['timestamp']),
                    'X-REQUEST-TYPE': 'JSAPI',
                    'X-REQUEST-NONCE': secret['nonce']
                }
            else:
                params['timestamp'] = str(int(time()))
                sign = sign_params(self._secret, params)

                return {
                    'X-REQUEST-KEY': self._key,
                    'X-REQUEST-SIGNATURE': sign,
                    'X-REQUEST-TIME': params['timestamp']
                }
        else:
            return {
                'X-REQUEST-KEY': self._key,
            }


    async def get_secret(self, method, pathname):
        key = '{}:{}'.format(method, pathname)
        expired_at = int(time()) - 250;
        secret = self._caches.get(key)
        if secret and secret['timestamp'] > expired_at:
            return secret

        secret = await self._makeSecret(method, pathname)
        secret['timestamp'] = int(secret['timestamp'])

        self._caches[key] = secret

        return secret


    def get_uri(self, pathname, query=None):
        if query is None:
            return '{}{}'.format(self._host, pathname)
        else:
            return '{}{}?{}'.format(self._host, pathname, urlencode(query))


    async def request(self, pathname, method='GET', query=None, data=None,
                      headers={}, is_json=False):
        params = {}
        if query:
            params.update(query.copy())
        if data:
            params.update(data.copy())

        secure = True
        method = method.upper()
        if method == 'GET' and not self._secure:
            secure = False

        headers = await self.get_headers(method, pathname, params, secure)
        if is_json:
            data = json.dumps(data)
            headers['content-type'] = 'application/json';

        headers['accept'] = 'application/json';
        url = self.get_uri(pathname, query);

        async with aiohttp.ClientSession() as client:
            async with client.request(method, url, headers=headers, data=data) as rsp:
                content = await rsp.read()

                try:
                    data = json.loads(str(content, 'utf-8'))
                    if data.get('err'):
                        raise ErrorResponse(data['err'])
                    if len(data.keys()) == 1:
                        return list(data.values()).pop()
                    return data
                except json.decoder.JSONDecodeError as e:
                    raise ErrorResponse(rsp.text)


def makeRequest(host, key, secret=None, makeSecret=None, secure=False):
    gw = Gateway(host, key, secret, makeSecret, secure)
    return gw.request

if __name__ == '__main__':
    pass
