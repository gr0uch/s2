const expectations = [
  "cba",
  "abcd",
  "zabc",
  "axyc",
  "cba",
  "ab",
  "(empty)",
  "ef",
];

async function test() {
  const list = document.querySelector("ul");
  const buttons = document.querySelectorAll(".try button");
  this.count = 0;
  this.total = buttons.length;
  for (let i = 0; i < buttons.length; i++) {
    const button = buttons[i];
    p.list.things = test.initialThings.slice();
    await frame();
    button.click();
    await frame();
    if (list.textContent === expectations[i]) this.count++;
    else {
      console.error(i, `expected:`, expectations[i], `got:`, list.textContent);
    }
  }
  p.list.things = test.initialThings.slice();
  await frame();
  this.runText = this.count === this.total ? "Tests passed!" : "Tests failed!";
}

function frame() {
  return new Promise((resolve) => requestAnimationFrame(resolve));
}

export default test;
