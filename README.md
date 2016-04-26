# hls-proxy

> A tool that if you're lucky you'll never need.

A collection of various unportable hacks to transform HLS streams for debugging
purposes. This is highly tailored towards the sort of setup I'm working with right now, so it will almost certainly not work out-of-the-box for you.

## Setup

The setup is a little gnarly and there isn't much I can do to change that. I'll assume here that you're setting up an Android device.

- Get [Charles](https://www.charlesproxy.com/) set up on your machine.
- Make sure your on the same WiFi as your phone.
- Install and trust the Charles TLS certificate. The recommended way of installing it through http://charlesproxy.com/getssl didn't work. I had to download it on a laptop, adb-push it to the device and manually install it there. YMMV.
- Enable SSL proxying in Charles (I think it's the default, but better double-check).
- Build this tool (see below) and start it on, let's say port 8080: `hls-proxy -- -p 8080 my-cdn.com`
- Set up a remote port mapping in Charles, by going to `Tools -> Map Remote` and set up a rule proxying to your local reverse proxy, e.g. "From: https://my-cdn.com:443/ -> http://localhost:8080"

Now use your app, device or whatever and start a stream. You should see the access log on stdout. If not, something went wrong with your Charles setup. Back to StackOverflow, sorry.

## Building

This tool is built with [stack](http://haskellstack.org).

```bash
git clone https://github.com/passy/hls-proxy
cd hls-proxy
stack setup
stack build
stack exec hls-proxy -- --help
```

## License

BSD-3
