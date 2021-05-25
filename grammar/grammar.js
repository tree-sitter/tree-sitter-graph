module.exports = grammar(require('tree-sitter-tsq/grammar'), {
  name: 'tsg',

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => 'hello'
  }
});
