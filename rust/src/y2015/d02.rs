use crate::util::format_soln_string;

fn surface_area_with_extra(lwh: [u32; 3]) -> u32 {
    2 * lwh[0] * lwh[1] + 2 * lwh[1] * lwh[2] + 2 * lwh[2] * lwh[0] + lwh[0] * lwh[1]
    // area of smallest side; lwh is pre-sorted
}

fn ribbon_with_extra(lwh: [u32; 3]) -> u32 {
    lwh[0] * 2 + lwh[1] * 2 + lwh[0] * lwh[1] * lwh[2]
}

pub fn soln(input: String) -> String {

    let presents = input.split('\n').into_iter();
    let mut total_surface_area: u32 = 0;
    let mut total_ribbon: u32 = 0;

    for p in presents {
        if p.len() > 2 {
            let dims = p.split('x').collect::<Vec<&str>>();
            let mut lwh: [u32; 3] = [
                dims[0].parse().unwrap(),
                dims[1].parse().unwrap(),
                dims[2].parse().unwrap(),
            ];
            lwh.sort();
            total_surface_area += surface_area_with_extra(lwh);
            total_ribbon += ribbon_with_extra(lwh);
        }
    }
    return format_soln_string(total_surface_area.to_string(), total_ribbon.to_string());
}
