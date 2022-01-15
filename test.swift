struct Pair<T> {
    let x: T;
    let y: T;
}

func identity<T>(_ x: T) -> T {
    return x;
}

func second<T>(_ pair: Pair<T>) -> T {
    return identity(pair.y);
}
