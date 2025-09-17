import { main } from "./test.js";

mocha.setup({
  timeout: '60000',
  ui: 'bdd'
});

main();

mocha.run();
