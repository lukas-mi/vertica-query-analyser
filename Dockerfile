FROM fpco/stack-build:lts-10.4 as build

RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build

FROM ubuntu:18.04

WORKDIR /opt/app
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-10.4/8.2.2/bin .
ENTRYPOINT ["./vq-analyser"]
