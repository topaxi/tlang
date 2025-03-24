export default {
  extends: ['stylelint-config-standard'],
  rules: {
    'custom-property-empty-line-before': null,
    'declaration-empty-line-before': null,
    'selector-class-pattern': null,
  },
  overrides: [
    {
      files: ['*.ts'],
      customSyntax: 'postcss-lit',
    },
  ],
};
