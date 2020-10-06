FROM alpine:3.8
ENV VERSION v1.1.0.1
RUN apk add --no-cache wget=1.20.3-r0 && \
    wget https://github.com/Lupino/yuntan-gateway/releases/download/$VERSION/simple-gateway-linux-$VERSION.tar.bz2 && \
    apk del wget && \
    tar xvf simple-gateway-linux-$VERSION.tar.bz2 && \
    mv simple-gateway /usr/bin && \
    rm simple-gateway*

COPY simple-gateway.yml /etc/simple-gateway.yml

EXPOSE 3000

ENTRYPOINT ["/usr/bin/simple-gateway"]

CMD ["-c", "/etc/simple-gateway.yml"]
