const expectations = [
  'cba',
  'abcd',
  'zabc',
  'axyc',
  'cba',
  'ab',
  '(empty)',
  'ef',
];

function test() {
  const list = document.querySelector('ul');
  const buttons = document.querySelectorAll('.try button');
  this.count = 0;
  this.total = buttons.length;
  buttons.forEach((button, i) => {
    p.list.things = test.initialThings.slice();
    button.click();
    if (list.textContent === expectations[i]) this.count++;
    else console.error(i, `expected:`, expectations[i], `got:`, list.textContent);
  });
  p.list.things = test.initialThings.slice();
  this.runText = this.count === this.total ? 'Tests passed!' : 'Tests failed!';
}

export default test;
