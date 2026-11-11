import { assert, describe, it } from 'vitest';

import { layeredLayout, type Point, type Size } from '../src/graph/layout.js';

const { equal } = assert;
const ok: (value: unknown, message?: string) => void = assert.ok;

// Tweety: A0 supports A2, A3 and A5, A5 supports A7, and A1 -> A4 -> A6.
const levels = new Map<string, number>([["A0", 0], ["A1", 0], ["A2", 1], ["A3", 1], ["A4", 1], ["A5", 1], ["A6", 2], ["A7", 2]]);
const supports = [
  { from: "A0", to: "A2" }, { from: "A0", to: "A3" }, { from: "A0", to: "A5" },
  { from: "A5", to: "A7" }, { from: "A1", to: "A4" }, { from: "A4", to: "A6" }
];

describe('Layered layout', function () {

  const positions = layeredLayout(levels, supports, { spacing: 100, levelSeparation: 80 });

  it('should put every level on its own row, facts at the bottom', function () {
    equal(positions.get("A0")!.y, 0);
    equal(positions.get("A2")!.y, -80);
    equal(positions.get("A7")!.y, -160);
  });

  it('should keep nodes on a row at least the spacing apart', function () {
    const row = ["A2", "A3", "A4", "A5"].map(it => positions.get(it)!.x).sort((a, b) => a - b);
    for (let i = 1; i < row.length; i++) ok(row[i] - row[i - 1] >= 100 - 1e-9);
  });

  it('should centre an argument under what it supports', function () {
    const middle = (positions.get("A2")!.x + positions.get("A3")!.x + positions.get("A5")!.x) / 3;
    ok(Math.abs(positions.get("A0")!.x - middle) < 1e-9);
  });

  it('should keep a chain straight', function () {
    equal(positions.get("A1")!.x, positions.get("A4")!.x);
    equal(positions.get("A4")!.x, positions.get("A6")!.x);
  });

  it('should be compact', function () {
    const xs = [...positions.values()].map(it => it.x);
    ok(Math.max(...xs) - Math.min(...xs) <= 3 * 100 + 1e-9);
  });

  it('should place a graph without supports on one row', function () {
    const flat = layeredLayout(new Map([["A0", 0], ["A1", 0]]), [], { spacing: 50 });
    equal(flat.get("A1")!.x - flat.get("A0")!.x, 50);
  });

  it('should pack separate parts side by side, one spacing apart', function () {
    const parts = layeredLayout(new Map([["A0", 0], ["A1", 1], ["A2", 0], ["A3", 1]]),
      [{ from: "A0", to: "A1" }, { from: "A2", to: "A3" }], { spacing: 100 });
    equal(parts.get("A2")!.x - parts.get("A0")!.x, 100);
  });

  it('should keep an attacker near what it attacks', function () {
    // A4 and A5 are facts on the bottom row; A5 attacks A3, the right end of the row above.
    const rows = new Map<string, number>([["A0", 0], ["A1", 1], ["A2", 1], ["A3", 1], ["A4", 0], ["A5", 0]]);
    const supports = [{ from: "A0", to: "A1" }, { from: "A0", to: "A2" }, { from: "A0", to: "A3" }];
    const placed = layeredLayout(rows, supports, { spacing: 100, attacks: [{ from: "A5", to: "A3" }, { from: "A4", to: "A1" }] });
    ok(Math.abs(placed.get("A5")!.x - placed.get("A3")!.x) < Math.abs(placed.get("A5")!.x - placed.get("A1")!.x));
  });

  it('should zig-zag a wide row so that it packs tighter', function () {
    // ten facts, all supporting the same argument above them
    const wide = new Map<string, number>([...Array.from({ length: 10 }, (_, i) => [`A${i}`, 0] as [string, number]), ["A10", 1]]);
    const supports = Array.from({ length: 10 }, (_, i) => ({ from: `A${i}`, to: "A10" }));
    const flat = layeredLayout(wide, supports, { spacing: 100, levelSeparation: 100, staggerFrom: 20 });
    const zigzag = layeredLayout(wide, supports, { spacing: 100, levelSeparation: 100, staggerFrom: 8 });
    const span = (layout: Map<string, Point>) => Math.max(...[...layout.values()].map(it => it.x)) - Math.min(...[...layout.values()].map(it => it.x));
    ok(span(zigzag) < 0.6 * span(flat));
    // neighbours on the row alternate between the two heights, and stay apart
    const xs = [...zigzag.entries()].filter(([id]) => id !== "A10").sort((a, b) => a[1].x - b[1].x);
    for (let i = 1; i < xs.length; i++) {
      ok(xs[i][1].y !== xs[i - 1][1].y);
      ok(Math.hypot(xs[i][1].x - xs[i - 1][1].x, xs[i][1].y - xs[i - 1][1].y) >= 60);
    }
  });

  it('should move rows apart to fill a tall area, never closer', function () {
    const rows = new Map<string, number>([["A0", 0], ["A1", 0], ["A2", 0], ["A3", 1]]);
    const supports = [{ from: "A0", to: "A3" }];
    const normal = layeredLayout(rows, supports, { spacing: 100, levelSeparation: 100 });
    const tall = layeredLayout(rows, supports, { spacing: 100, levelSeparation: 100, aspect: 1 });
    const wide = layeredLayout(rows, supports, { spacing: 100, levelSeparation: 100, aspect: 0.1 });
    ok(tall.get("A3")!.y < normal.get("A3")!.y);
    equal(wide.get("A3")!.y, normal.get("A3")!.y);
  });

  describe('with node sizes', function () {

    interface Box { id: string; left: number; right: number; top: number; bottom: number }

    // Twelve facts of different widths supporting one argument, which is tall.
    const ids = Array.from({ length: 12 }, (_, i) => `A${i}`);
    const rows = new Map<string, number>([...ids.map((id): [string, number] => [id, 0]), ["A12", 1]]);
    const supports = ids.map(id => ({ from: id, to: "A12" }));
    const sizes = new Map<string, Size>([...ids.map((id, i): [string, Size] => [id, { width: 40 + (i % 4) * 60, height: 30 }]), ["A12", { width: 120, height: 90 }] as [string, Size]]);
    const boxes = (layout: Map<string, Point>) => [...layout].map(([id, p]): Box => ({ id, left: p.x - sizes.get(id)!.width / 2, right: p.x + sizes.get(id)!.width / 2,
      top: p.y - sizes.get(id)!.height / 2, bottom: p.y + sizes.get(id)!.height / 2 }));
    const overlap = (a: Box, b: Box) => a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom;

    for (const staggerFrom of [20, 8]) {
      it(`should never overlap two nodes (${staggerFrom === 8 ? 'zig-zag' : 'straight'} row)`, function () {
        const all = boxes(layeredLayout(rows, supports, { sizes, gap: 10, staggerFrom }));
        for (let i = 0; i < all.length; i++)
          for (let j = i + 1; j < all.length; j++)
            ok(!overlap(all[i], all[j]), `${all[i].id} overlaps ${all[j].id}`);
      });
    }
  });
});
