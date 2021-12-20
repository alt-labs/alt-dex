/** @type {import('next').NextConfig} */
module.exports = {
  reactStrictMode: true,
  images: {
    domains: ['ucarecdn.com', 'assets.coingecko.com'],
  },
  future: {
    webpack5: true,
  },
  webpack(config) {
    config.output.webassemblyModuleFilename = 'static/wasm/[modulehash].wasm';
    config.experiments = { syncWebAssembly: true, layers: true };

    return config;
  },
};
