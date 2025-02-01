import { main } from "./test.js";

mocha.setup({
  reporter: 'spec',
  // reporter-options: '',
  timeout: '60000',
  ui: 'bdd'
});

main();

mocha.run();
