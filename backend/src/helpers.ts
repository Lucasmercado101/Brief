function partition<T, X extends T, Y extends T>(
  arr: T[],
  predicate: (element: T) => element is X
): [X[], Y[]] {
  const res1: X[] = [];
  const res2: Y[] = [];

  arr.forEach((element) => {
    if (predicate(element)) {
      res1.push(element);
    } else {
      res2.push(element as Y);
    }
  });
  return [res1, res2];
}
