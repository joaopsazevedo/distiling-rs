use crate::Strategy::{Max, Simple};
use anyhow::{anyhow, bail, ensure, Context};
use std::collections::{HashMap, HashSet};
use std::env;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Coordinate {
    l: usize,
    c: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Strategy {
    Simple,
    GreatThan(usize),
    Max,
}

impl Display for Strategy {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Simple => write!(f, "-1"),
            Strategy::GreatThan(v) => write!(f, "{}", v),
            Max => write!(f, "-3"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Solution {
    deleted_blocks: Vec<Coordinate>,
    score: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ProblemState {
    base: ProblemBaseState,
    helper: ProblemHelperState,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ProblemBaseState {
    tiles: Vec<isize>,
    score: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ProblemHelperState {
    tiles_kind_counter: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Problem {
    l: usize,
    c: usize,
    v: Strategy,
    visited: Vec<bool>,
    initial_state: ProblemState,
}

impl Problem {
    fn new<I>(numbers: &mut I) -> Result<Self, anyhow::Error>
    where
        I: Iterator<Item = isize>,
    {
        let l = numbers.next().context("Failed to get L")?;
        let c = numbers.next().context("Failed to get C")?;
        let v = numbers.next().context("Failed to get v")?;
        ensure!(l > 0, "Expected a positive L ({})", l);
        ensure!(c > 0, "Expected a positive C ({})", c);
        let l = l as usize;
        let c = c as usize;
        let v = match v {
            -3 => Max,
            -1 => Simple,
            0..=isize::MAX => Strategy::GreatThan(v as usize),
            _ => bail!("Expected a v ({}) equal to -3, -1 or positive", v),
        };
        let tiles = numbers.take(c * l).collect::<Vec<_>>();
        ensure!(tiles.len() == c * l, "Invalid problem definition");
        let mut tiles_kind_counter = vec![0; *tiles.iter().max().unwrap_or(&0).max(&0) as usize];
        tiles.iter().for_each(|&tile| {
            if tile > 0 {
                tiles_kind_counter[tile as usize - 1] += 1;
            }
        });
        let visited = vec![false; c * l];
        let base_state = ProblemBaseState { tiles, score: 0 };
        let helper_state = ProblemHelperState { tiles_kind_counter };
        let initial_state = ProblemState {
            base: base_state,
            helper: helper_state,
        };
        Ok(Self {
            l,
            c,
            v,
            visited,
            initial_state,
        })
    }

    fn visited_at(&self, coordinate: &Coordinate) -> Result<&bool, anyhow::Error> {
        self.visited
            .get(coordinate.l * self.c + coordinate.c)
            .with_context(|| format!("Can't get visited at ({:?})", coordinate))
    }

    fn visited_at_mut(&mut self, coordinate: &Coordinate) -> Result<&mut bool, anyhow::Error> {
        self.visited
            .get_mut(coordinate.l * self.c + coordinate.c)
            .with_context(|| format!("Can't get visited at ({:?})", coordinate))
    }

    fn reset_visited(&mut self) {
        self.visited.iter_mut().for_each(|v| *v = false);
    }

    fn at<'a>(
        &self,
        state: &'a ProblemState,
        coordinate: &Coordinate,
    ) -> Result<&'a isize, anyhow::Error> {
        state
            .base
            .tiles
            .get(coordinate.l * self.c + coordinate.c)
            .with_context(|| format!("Can't get visited at ({:?})", coordinate))
    }

    fn at_mut<'a>(
        &mut self,
        state: &'a mut ProblemState,
        coordinate: &Coordinate,
    ) -> Result<&'a mut isize, anyhow::Error> {
        state
            .base
            .tiles
            .get_mut(coordinate.l * self.c + coordinate.c)
            .with_context(|| format!("Can't get visited at ({:?})", coordinate))
    }

    fn neighbours(&self, coordinate: &Coordinate) -> Vec<Coordinate> {
        vec![
            Coordinate {
                l: coordinate.l,
                c: coordinate.c.saturating_sub(1),
            },
            Coordinate {
                l: coordinate.l,
                c: coordinate.c + 1,
            },
            Coordinate {
                l: coordinate.l.saturating_sub(1),
                c: coordinate.c,
            },
            Coordinate {
                l: coordinate.l + 1,
                c: coordinate.c,
            },
        ]
        .into_iter()
        .filter(|n| *n != *coordinate && n.c < self.c && n.l < self.l)
        .collect()
    }

    fn set_as_visited(&mut self, coordinate: &Coordinate) -> Result<(), anyhow::Error> {
        *self.visited_at_mut(coordinate)? = true;
        Ok(())
    }

    fn delete_tile(
        &mut self,
        state: &mut ProblemState,
        coordinate: &Coordinate,
    ) -> Result<(), anyhow::Error> {
        Ok(*self.at_mut(state, coordinate)? = -1)
    }

    fn step(
        &mut self,
        state: &mut ProblemState,
        coordinate: &Coordinate,
    ) -> Result<usize, anyhow::Error> {
        self.reset_visited();
        let tile_value = self.at(state, coordinate)?.clone();
        ensure!(tile_value > 0, "unexpected tile value");
        self.delete(state, coordinate).and_then(|deleted| {
            ensure!(deleted > 1, "expected to delete more than 1 tile");
            let score = deleted * (deleted - 1);
            self.vertical_slide(state)?;
            self.horizontal_slide(state)?;
            state.helper.tiles_kind_counter[tile_value as usize - 1] -= deleted;
            state.base.score += score;
            Ok(score)
        })
    }

    fn delete(
        &mut self,
        state: &mut ProblemState,
        coordinate: &Coordinate,
    ) -> Result<usize, anyhow::Error> {
        let target = *self.at(state, coordinate)?;
        self.delete_rec(state, coordinate, target)
    }

    fn delete_rec(
        &mut self,
        state: &mut ProblemState,
        coordinate: &Coordinate,
        target: isize,
    ) -> Result<usize, anyhow::Error> {
        self.set_as_visited(coordinate)?;
        self.delete_tile(state, coordinate)?;
        self.neighbours(coordinate)
            .iter()
            .try_fold(1, |acc, neighbour_coordinate| {
                let neighbour_value = *self.at(state, neighbour_coordinate)?;
                if neighbour_value == target && !*self.visited_at(neighbour_coordinate)? {
                    Ok(acc + self.delete_rec(state, neighbour_coordinate, target)?)
                } else {
                    Ok(acc)
                }
            })
    }

    fn vertical_slide(&mut self, state: &mut ProblemState) -> Result<(), anyhow::Error> {
        for c in 0..self.c {
            for l in (1..self.l).rev() {
                let current_tile_coordinate = Coordinate { l, c };
                let current_tile_value = *self.at(state, &current_tile_coordinate)?;
                if current_tile_value == -1 {
                    for l_top in (0..=l - 1).rev() {
                        let top_tile_coordinate = Coordinate { l: l_top, c };
                        let top_tile_value = *self.at(state, &top_tile_coordinate)?;
                        if top_tile_value != -1 {
                            *self.at_mut(state, &current_tile_coordinate)? = top_tile_value;
                            *self.at_mut(state, &top_tile_coordinate)? = -1;
                            break;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn horizontal_slide(&mut self, state: &mut ProblemState) -> Result<(), anyhow::Error> {
        for c in (1..self.c).rev() {
            let bottom_tile_coordinate = Coordinate { l: self.l - 1, c };
            let bottom_tile_value = *self.at(state, &bottom_tile_coordinate)?;
            if bottom_tile_value == -1 {
                for i in (0..=c - 1).rev() {
                    let left_tile_coordinate = Coordinate {
                        l: self.l - 1,
                        c: i,
                    };
                    let left_tile_value = *self.at(state, &left_tile_coordinate)?;
                    if left_tile_value != -1 {
                        for l in 0..self.l {
                            let left_coordinate = Coordinate { l, c: i };
                            let right_coordinate = Coordinate { l, c };
                            let left_value = *self.at(state, &left_coordinate)?;
                            *self.at_mut(state, &right_coordinate)? = left_value;
                            *self.at_mut(state, &left_coordinate)? = -1;
                        }
                        break;
                    }
                }
            }
        }
        Ok(())
    }

    fn get_deletable_block(
        &self,
        state: &ProblemState,
    ) -> Result<Option<Coordinate>, anyhow::Error> {
        for l in (0..self.l).rev() {
            for c in 0..self.c {
                let tile = Coordinate { l, c };
                let tile_value = *self.at(state, &tile)?;
                if tile_value != -1
                    && self
                        .neighbours(&tile)
                        .into_iter()
                        .map(|n| self.at(state, &n))
                        .collect::<Result<Vec<_>, _>>()?
                        .into_iter()
                        .any(|n| *n == tile_value)
                {
                    return Ok(Some(tile));
                }
            }
        }
        Ok(None)
    }

    fn get_all_deletable_blocks(
        &mut self,
        state: &ProblemState,
    ) -> Result<Vec<Coordinate>, anyhow::Error> {
        self.reset_visited();
        let mut deletable_blocks = vec![];
        for l in (0..self.l).rev() {
            for c in 0..self.c {
                let tile = Coordinate { l, c };
                if !*self.visited_at(&tile)? {
                    self.set_as_visited(&tile)?;
                    let tile_value = *self.at(state, &tile)?;
                    if tile_value == -1 {
                        continue;
                    }
                    let mut block_size = 1;
                    let mut neighbours_to_explore = self.neighbours(&tile);
                    while let Some(neighbour) = neighbours_to_explore.pop() {
                        if !*self.visited_at(&neighbour)?
                            && *self.at(state, &neighbour)? == tile_value
                        {
                            block_size += 1;
                            self.set_as_visited(&neighbour)?;
                            neighbours_to_explore.extend(self.neighbours(&neighbour));
                        }
                    }
                    if block_size != 1 {
                        deletable_blocks.push(tile);
                    }
                }
            }
        }
        Ok(deletable_blocks)
    }

    fn best_possible_score(state: &ProblemState) -> usize {
        state.helper.tiles_kind_counter.iter().fold(0, |acc, n| {
            if *n == 0 {
                acc
            } else {
                acc + n * (n - 1)
            }
        })
    }

    fn solve(&mut self) -> Result<(), anyhow::Error> {
        match self.v {
            Simple => self.solve_simple(),
            Strategy::GreatThan(v) => self.solve_greater_than(v),
            Max => self.solve_max(),
        }
    }

    fn solve_simple(&mut self) -> Result<(), anyhow::Error> {
        let mut solution = Solution {
            deleted_blocks: vec![],
            score: 0,
        };
        let mut state = self.initial_state.clone();
        while let Some(deletable_block) = self.get_deletable_block(&mut state)? {
            let deleted = self.step(&mut state, &deletable_block)?;
            solution.score += deleted * (deleted - 1);
            solution.deleted_blocks.push(deletable_block);
        }
        println!("{} {} {}", self.l, self.c, self.v);
        println!("{} {}", solution.deleted_blocks.len(), solution.score);
        solution
            .deleted_blocks
            .iter()
            .for_each(|deleted| println!("{} {}", self.l - deleted.l, deleted.c + 1));
        println!();
        Ok(())
    }

    fn solve_greater_than_dfs(
        &mut self,
        target: usize,
        state: ProblemState,
        solution: &mut Solution,
        cache: &mut HashSet<ProblemBaseState>,
    ) -> Result<Option<()>, anyhow::Error> {
        if cache.contains(&state.base) {
            return Ok(None);
        }

        if solution.score + Self::best_possible_score(&state) < target {
            cache.insert(state.base);
            return Ok(None);
        }

        for deletable_block in self.get_all_deletable_blocks(&state)? {
            let mut new_state = state.clone();
            let score = self.step(&mut new_state, &deletable_block)?;

            solution.score += score;
            solution.deleted_blocks.push(deletable_block);

            if solution.score >= target {
                cache.insert(state.base);
                return Ok(Some(()));
            }

            if self
                .solve_greater_than_dfs(target, new_state, solution, cache)?
                .is_some()
            {
                cache.insert(state.base);
                return Ok(Some(()));
            } else {
                solution.score -= score;
                solution.deleted_blocks.pop();
            }
        }

        cache.insert(state.base);
        Ok(None)
    }

    fn solve_greater_than(&mut self, v: usize) -> Result<(), anyhow::Error> {
        let mut solution = Solution {
            score: 0,
            deleted_blocks: vec![],
        };
        let mut cache = HashSet::new();
        let state = self.initial_state.clone();
        match self.solve_greater_than_dfs(v, state, &mut solution, &mut cache)? {
            Some(_) => {
                println!("{} {} {}", self.l, self.c, self.v);
                println!("{} {}", solution.deleted_blocks.len(), solution.score);
                solution.deleted_blocks.iter().for_each(|deleted| {
                    println!("{} {}", self.l - deleted.l, deleted.c + 1);
                });
            }
            None => {
                println!("{} {} {}", self.l, self.c, self.v);
                println!("0 -1");
            }
        }
        println!();
        Ok(())
    }

    fn solve_max_dfs(
        &mut self,
        state: ProblemState,
        cache: &mut HashMap<ProblemBaseState, Option<Solution>>,
        best_score_so_far: &mut usize,
    ) -> Result<Option<Solution>, anyhow::Error> {
        if let Some(solution) = cache.get(&state.base) {
            return Ok(solution.clone());
        }

        let deletable_blocks = self.get_all_deletable_blocks(&state)?;
        if deletable_blocks.is_empty() {
            *best_score_so_far = (*best_score_so_far).max(state.base.score);
            return Ok(Some(Solution {
                deleted_blocks: vec![],
                score: 0,
            }));
        }

        let best_solution = deletable_blocks
            .into_iter()
            .map(
                |deletable_block| -> Result<Option<Solution>, anyhow::Error> {
                    let mut new_state = state.clone();
                    let score = self.step(&mut new_state, &deletable_block)?;

                    if new_state.base.score + Self::best_possible_score(&new_state)
                        <= *best_score_so_far
                    {
                        return Ok(None);
                    }

                    if let Some(mut new_problem_solution) =
                        self.solve_max_dfs(new_state, cache, best_score_so_far)?
                    {
                        new_problem_solution.score += score;
                        new_problem_solution.deleted_blocks.push(deletable_block);
                        Ok(Some(new_problem_solution))
                    } else {
                        Ok(None)
                    }
                },
            )
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .filter_map(|s| s)
            .max_by(|l, r| l.score.cmp(&r.score));
        cache.insert(state.base, best_solution.clone());
        Ok(best_solution)
    }

    fn solve_max(&mut self) -> Result<(), anyhow::Error> {
        let mut cache = HashMap::new();
        let mut best_score_so_far = 0;
        let state = self.initial_state.clone();
        let solution = self
            .solve_max_dfs(state, &mut cache, &mut best_score_so_far)?
            .ok_or(anyhow!("failed to compute maximum solution"))?;

        println!("{} {} {}", self.l, self.c, self.v);
        println!("{} {}", solution.deleted_blocks.len(), solution.score);
        solution
            .deleted_blocks
            .into_iter()
            .rev()
            .for_each(|deleted| {
                println!("{} {}", self.l - deleted.l, deleted.c + 1);
            });
        println!();
        Ok(())
    }
}

fn main() -> Result<(), anyhow::Error> {
    let args: Vec<String> = env::args().collect();

    ensure!(args.len() == 2, "Invalid arguments");

    let input_filename = args.get(1).context("Invalid arguments")?;
    let mut file = File::open(input_filename)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let numbers = contents
        .split_whitespace()
        .map(|s| s.parse::<isize>())
        .collect::<Result<Vec<_>, _>>()?;
    let mut numbers_iter = numbers.into_iter().peekable();

    while numbers_iter.peek().is_some() {
        let mut problem = Problem::new(&mut numbers_iter)?;
        problem.solve()?;
    }

    Ok(())
}
