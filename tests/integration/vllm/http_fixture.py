"""Loopback-only independent OpenAI-compatible transport fixture."""
from http.server import BaseHTTPRequestHandler, HTTPServer
import json
from socketserver import TCPServer


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        model = request['model']
        if model == 'auth-failure':
            self.send_response(401)
            self.end_headers()
            return
        response = dict(model=model, choices=[dict(finish_reason='stop', message=dict(content='loopback reply'))])
        data = json.dumps(response).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def log_message(self, *args):
        pass


class LoopbackServer(HTTPServer):
    def server_bind(self):
        # The fixture has an explicit loopback address; reverse DNS is neither
        # needed nor part of testing the HTTP protocol.
        TCPServer.server_bind(self)
        self.server_name = 'localhost'
        self.server_port = self.server_address[1]


server = LoopbackServer(('127.0.0.1', 0), Handler)
print(server.server_port, flush=True)
server.serve_forever()
