import esbuild from "esbuild";
import http from "node:http";
import handleSSE from "./sse-server.mjs";

const result = await esbuild.serve(
  {
    servedir: ".",
  },
  {
    entryPoints: ["src/main.mjs"],
    bundle: true,
    outdir: "dist",
    outExtension: {
      ".js": ".mjs",
    },
    inject: ["dev-client.mjs"],
    format: "esm",
    target: "esnext",
  }
);

const STATIC_REGEXP = /\.mjs(\?.*)?$/;
const DEV_PORT = 1337;

const { host: hostname, port } = result;
const devServer = http.createServer((req, res) => {
  const { url, method, headers } = req;

  if (method === "GET" && url.startsWith("/dev-events")) {
    return handleSSE(req, res);
  }

  // Rewrite most paths in the dev server to the index :)
  const path = STATIC_REGEXP.test(url) ? url : "/";

  const options = {
    hostname,
    port,
    path,
    method,
    headers,
  };

  // Forward each incoming request to esbuild
  const proxyReq = http.request(options, (proxyRes) => {
    res.writeHead(proxyRes.statusCode, proxyRes.headers);
    proxyRes.pipe(res, { end: true });
  });

  // Forward the body of the request to esbuild
  req.pipe(proxyReq, { end: true });
});

devServer.listen(DEV_PORT, () => {
  console.log(
    `dev server listening on ${hostname}:${DEV_PORT}...`
  );
});
