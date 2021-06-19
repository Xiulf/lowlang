func identity<T>(_ x: T) -> T {
    let y = x;
    return identity2(y);
}

func identity2<T>(_ x: T) -> T {
    return x;
}

identity(22);
