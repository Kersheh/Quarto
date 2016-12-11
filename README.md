Quarto (Functionally Designed)
===
A (mostly) functional game client of Quarto designed for the browser. The game is intended to connect over IRC with another Quarto game client.

Requirements
---
- Node.js

Install & Run
---
- `npm install`
- `npm run server`

Note: node-irc supports character set detection using [icu](http://site.icu-project.org/). You'll need to install libiconv and libicu in order to use this feature. If you do not have these libraries or their headers installed, you will receive errors when trying to build these dependencies. **However, node-irc should still install and you'll be able to use it, just not the character set features.**

Usage
---
The IRC can be configured with `server.js`. The application can be accessed at its default location http://localhost:8080/.
