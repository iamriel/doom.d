const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const { test } = require('node:test');

// Export my-emacs-everywhere--capture-script with batch Emacs, then pass
// its path in EE_CAPTURE_SCRIPT. These tests never control a real app.
const script = fs.readFileSync(process.env.EE_CAPTURE_SCRIPT, 'utf8');

function capture(options = {}) {
  const events = [];
  const clipboard = {
    changeCount: 42,
    stringForType: () => options.copiedText,
  };
  const field = {
    attributes: {
      byName: (attribute) => ({
        value: () => {
          if (attribute === 'AXRole') return options.fieldRole || 'AXTextArea';
          if (options.noAccessibilityValue) throw Error("Can't get object. (-1728)");
          return options.fieldText;
        },
      }),
    },
  };
  const app = {
    name: () => options.appName || 'Arc',
    attributes: {
      byName: () => ({
        value: () => {
          if (options.noFocusedElement) throw Error("Can't get object. (-1728)");
          return field;
        },
      }),
    },
  };
  const systemEvents = {
    applicationProcesses: {
      whose: (filter) => {
        assert.equal(filter.frontmost, true);
        if (options.noFrontmostProcess) throw Error("Can't get object. (-1728)");
        if (options.noActiveApplication) return [];
        return [app];
      },
      byName: () => assert.fail('Application IDs must not be used as process names'),
    },
    keystroke: (key, modifiers) => {
      assert.deepEqual(Array.from(modifiers.using), ['command down']);
      events.push(key);
      if (options.keystrokeError) throw Error(options.keystrokeError);
      if (key === 'c' && !options.unchangedClipboard) clipboard.changeCount++;
    },
  };
  const context = vm.createContext({
    Application: () => systemEvents,
    ObjC: { import: () => {}, unwrap: (value) => value },
    $: { NSPasteboard: { generalPasteboard: clipboard }, NSPasteboardTypeString: 'text' },
    delay: () => {},
  });
  vm.runInContext(script, context);
  return { run: () => context.run(['a-window-id-is-not-a-process-name']), events };
}

test('imports the complete accessible field and selects it for replacement', () => {
  const text = 'λ 😀 "quoted" \\slash\t  \nlast line  ';
  const check = capture({ fieldText: text });
  assert.equal(JSON.parse(check.run()), text);
  assert.deepEqual(check.events, ['a']);
});

test('an inaccessible Arc field falls back to a fresh full-text copy', () => {
  const text = 'λ 😀 "quoted" \\slash\t  \n'.repeat(20000) + 'last line  ';
  const check = capture({ noAccessibilityValue: true, copiedText: text });
  assert.equal(JSON.parse(check.run()), text);
  assert.deepEqual(check.events, ['a', 'c']);
});

test('an empty accessible textbox never imports stale clipboard text', () => {
  const check = capture({ fieldText: '', copiedText: 'stale clipboard' });
  assert.equal(JSON.parse(check.run()), '');
  assert.deepEqual(check.events, ['a']);
});

test('an unsuccessful copy refuses stale clipboard contents', () => {
  const check = capture({ noAccessibilityValue: true, unchangedClipboard: true,
                          copiedText: 'stale clipboard' });
  assert.equal(JSON.parse(check.run()), null);
});

test('Emacs itself cannot be selected as the source application', () => {
  const check = capture({ appName: 'Emacs' });
  assert.equal(JSON.parse(check.run()), null);
  assert.deepEqual(check.events, []);
});

test('process lookup failures allow an empty session', () => {
  const check = capture({ noFrontmostProcess: true });
  assert.equal(JSON.parse(check.run()), null);
  assert.deepEqual(check.events, []);
});

test('keystroke failures allow an empty session', () => {
  const check = capture({ fieldText: 'text', keystrokeError: "Can't get object. (-1728)" });
  assert.equal(JSON.parse(check.run()), null);
});

test('no active application allows an empty session without keystrokes', () => {
  const check = capture({ noActiveApplication: true });
  assert.equal(JSON.parse(check.run()), null);
  assert.deepEqual(check.events, []);
});

test('no focused textbox allows an empty session without keystrokes', () => {
  const check = capture({ noFocusedElement: true });
  assert.equal(JSON.parse(check.run()), null);
  assert.deepEqual(check.events, []);
});

test('a focused web page is never selected or copied as textbox content', () => {
  const check = capture({ fieldRole: 'AXWebArea', fieldText: 'page content' });
  assert.equal(JSON.parse(check.run()), null);
  assert.deepEqual(check.events, []);
});
