/** @type {import('vite').UserConfig} */
export default {
  root: './src/Client',
  build: {
    outDir: '../../deploy/public',
  },
  publicDir: 'public',
  server: {
    proxy: {
        "/api": "http://localhost:8085"
    }
  }
}