let rec hanoi = (n, a, b, c) =>
  if (n > 0) {
    hanoi(n - 1, a, c, b);
    print({j|Move disk from pole $a to pole $b|j});
    hanoi(n - 1, c, b, a);
  };

hanoi(4, 1, 2, 3);
