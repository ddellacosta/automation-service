// Represents a handler that Playwright calls when the page opens a WebSocket.
// We store the WebSocketRoute so PureScript can send messages into it later.

export const routeWebSocket_ = (page) => (urlPattern) => (onRoute) => () => {
  return page.routeWebSocket(urlPattern, (ws) => {
    // Prevent connecting to the real server
    // (just don't call ws.connectToServer())

    // Call the PureScript handler with the WebSocketRoute
    onRoute(ws)();
  });
};

// Send a message from "server" to the page
export const sendToPage_ = (wsRoute) => (msg) => () => {
  wsRoute.send(msg);
};

// Register a handler for messages the page sends to the "server"
export const onMessage_ = (wsRoute) => (handler) => () => {
  wsRoute.onMessage((msg) => {
    handler(msg)();
  });
};

// Register a handler for when the page closes the WebSocket
export const onClose_ = (wsRoute) => (handler) => () => {
  wsRoute.onClose((code, reason) => {
    handler(code)(reason)();
  });
};

// Close the server side of the route
export const closeRoute_ = (wsRoute) => (opts) => () => {
  wsRoute.close(opts);
};
