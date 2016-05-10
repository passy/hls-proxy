from mitmproxy.models import HTTPResponse
from netlib.http import Headers

def request(context, flow):
    if flow.request.pretty_host.endswith('myhost.com'):
        flow.request.host = 'localhost'
        flow.request.port = 8080
        flow.request.scheme = 'http'
