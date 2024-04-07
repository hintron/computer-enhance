pub mod json;
pub mod profile;
#[cfg(test)]
mod tests;

/// Compute the haversine distance between two points on a sphere with the given
/// radius. x and y points are in radians.
///
/// See https://en.wikipedia.org/wiki/Haversine_formula
///
/// d = distance along sphere between two points
/// r = radius of sphere
/// theta = d / r
/// hav(theta) = (sin(theta / 2)) ^ 2 = (1 - cos(theta)) / 2
/// hav(theta) = hav(y2 - y1) + cos(y1) * cos(y2) * hav(x2 - x1)
///
/// So start with:
/// hav(theta) = hav(theta)
/// sin(theta / 2) ^ 2 = hav(y2 - y1) + cos(y1) * cos(y2) * hav(x2 - x1)
/// sin((d / r) / 2) ^ 2 = sin((y2 - y1) / 2) ^ 2 + cos(y1) * cos(y2) * sin((x2 - x1) / 2) ^ 2
///
/// This is the final equation to solve for d:
/// d = 2 * r * arcsin(sqrt(sin((y2 - y1) / 2) ^ 2 + cos(y1) * cos(y2) * sin((x2 - x1) / 2) ^ 2))
fn get_haversine_distance_rads(radius: f64, x1: f64, y1: f64, x2: f64, y2: f64) -> f64 {
    // Compute arcsin(sqrt(sin((y2 - y1) / 2) ^ 2 + cos(y1) * cos(y2) * sin((x2 - x1) / 2) ^ 2))
    let arcsin = (((y2 - y1) / 2.0).sin().powi(2)
        + y1.cos() * y2.cos() * ((x2 - x1) / 2.0).sin().powi(2))
    .sqrt()
    .asin();
    // Compute d = 2 * r * arcsin
    let distance = 2.0 * radius * arcsin;
    distance
}

/// Compute the haversine distance between two points on a sphere with the given
/// radius. x and y points are in degrees, from -180 to 180, and y points are from
/// -90 to 90.
pub fn get_haversine_distance(radius: f64, x1: f64, y1: f64, x2: f64, y2: f64) -> f64 {
    get_haversine_distance_rads(
        radius,
        x1.to_radians(),
        y1.to_radians(),
        x2.to_radians(),
        y2.to_radians(),
    )
}
