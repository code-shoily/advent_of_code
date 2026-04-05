defmodule AdventOfCode.Y2020.Day20 do
  @moduledoc """
  --- Day 20: Jurassic Jigsaw ---
  Problem Link: https://adventofcode.com/2020/day/20
  Difficulty: xl
  Tags: geometry rotation
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2020, 20)

  def run(input \\ input()) do
    tiles = parse(input)
    grid_side = floor(:math.sqrt(map_size(tiles)))

    # Classification to speed up solver
    tile_types = get_tile_types(tiles)

    # Solve the jigsaw
    assembled_grid = solve_jigsaw(tiles, grid_side, tile_types)

    # Part 1: Corner product
    p1 =
      [
        assembled_grid[{0, 0}],
        assembled_grid[{0, grid_side - 1}],
        assembled_grid[{grid_side - 1, 0}],
        assembled_grid[{grid_side - 1, grid_side - 1}]
      ]
      |> Enum.map(&elem(&1, 0))
      |> Enum.product()

    # Part 2: Sea monsters
    final_image = build_full_image(assembled_grid, grid_side)
    p2 = calculate_roughness(final_image)

    {p1, p2}
  end

  def parse(input) do
    input
    |> String.split(["\n\n", "\r\n\r\n"], trim: true)
    |> Map.new(fn block ->
      [line1 | tile_lines] = String.split(block, "\n", trim: true)
      id = Regex.run(~r/\d+/, line1) |> List.first() |> String.to_integer()
      grid = Enum.map(tile_lines, &String.graphemes/1)
      {id, grid}
    end)
  end

  defp get_tile_types(tiles) do
    edge_counts =
      tiles
      |> Enum.flat_map(fn {_, grid} ->
        [hd(grid), List.last(grid), Enum.map(grid, &hd/1), Enum.map(grid, &List.last/1)]
        |> Enum.map(&canonical_edge/1)
      end)
      |> Enum.frequencies()

    Map.new(tiles, fn {id, grid} ->
      uniques =
        [hd(grid), List.last(grid), Enum.map(grid, &hd/1), Enum.map(grid, &List.last/1)]
        |> Enum.count(fn e -> edge_counts[canonical_edge(e)] == 1 end)

      type =
        case uniques do
          2 -> :corner
          1 -> :edge
          0 -> :middle
        end

      {id, type}
    end)
  end

  defp canonical_edge(e) do
    [Enum.join(e), Enum.join(Enum.reverse(e))] |> Enum.min()
  end

  defp solve_jigsaw(tiles, side, tile_types) do
    tile_variants = Map.new(tiles, fn {id, grid} -> {id, all_orientations(grid)} end)
    do_solve(tile_variants, %{}, Map.keys(tiles), 0, 0, side, tile_types)
  end

  defp do_solve(_, grid, [], _, _, _, _), do: grid

  defp do_solve(variants, grid, available, r, c, side, types) do
    {next_r, next_c} = if c + 1 == side, do: {r + 1, 0}, else: {r, c + 1}

    is_r_edge = r == 0 or r == side - 1
    is_c_edge = c == 0 or c == side - 1

    needed_type =
      case {is_r_edge, is_c_edge} do
        {true, true} -> :corner
        {true, false} -> :edge
        {false, true} -> :edge
        {false, false} -> :middle
      end

    available_to_try = Enum.filter(available, fn id -> types[id] == needed_type end)

    Enum.find_value(available_to_try, fn id ->
      Enum.find_value(variants[id], fn variant ->
        if matches?(grid, variant, r, c) do
          do_solve(
            variants,
            Map.put(grid, {r, c}, {id, variant}),
            List.delete(available, id),
            next_r,
            next_c,
            side,
            types
          )
        end
      end)
    end)
  end

  defp matches?(placed, variant, r, c) do
    top_match =
      case placed[{r - 1, c}] do
        nil -> true
        {_, neighbor} -> List.last(neighbor) == hd(variant)
      end

    left_match =
      case placed[{r, c - 1}] do
        nil -> true
        {_, neighbor} -> Enum.map(neighbor, &List.last/1) == Enum.map(variant, &hd/1)
      end

    top_match and left_match
  end

  defp all_orientations(grid) do
    g0 = grid
    g1 = rotate(g0)
    g2 = rotate(g1)
    g3 = rotate(g2)
    f0 = Enum.reverse(grid)
    f1 = rotate(f0)
    f2 = rotate(f1)
    f3 = rotate(f2)
    [g0, g1, g2, g3, f0, f1, f2, f3] |> Enum.uniq()
  end

  defp rotate(grid) do
    grid |> Enum.zip() |> Enum.map(fn row -> row |> Tuple.to_list() |> Enum.reverse() end)
  end

  defp build_full_image(grid, side) do
    for r <- 0..(side - 1) do
      for row_in_tile <- 1..8 do
        for c <- 0..(side - 1) do
          {_, tile_grid} = grid[{r, c}]
          tile_grid |> Enum.at(row_in_tile) |> Enum.slice(1..8)
        end
        |> Enum.concat()
      end
    end
    |> Enum.concat()
  end

  @monster_offsets [
    {0, 18},
    {1, 0},
    {1, 5},
    {1, 6},
    {1, 11},
    {1, 12},
    {1, 17},
    {1, 18},
    {1, 19},
    {2, 1},
    {2, 4},
    {2, 7},
    {2, 10},
    {2, 13},
    {2, 16}
  ]
  @monster_h 3
  @monster_w 20

  defp calculate_roughness(image) do
    total_hashes = Enum.count(List.flatten(image), &(&1 == "#"))

    Enum.find_value(all_orientations(image), fn variant ->
      monsters = count_monsters(variant)
      if monsters > 0, do: total_hashes - monsters * 15
    end)
  end

  defp count_monsters(image) do
    h = length(image)
    w = length(hd(image))

    for r <- 0..(h - @monster_h), c <- 0..(w - @monster_w), monster_at?(image, r, c), reduce: 0 do
      acc -> acc + 1
    end
  end

  defp monster_at?(image, r, c) do
    Enum.all?(@monster_offsets, fn {dr, dc} ->
      image |> Enum.at(r + dr) |> Enum.at(c + dc) == "#"
    end)
  end
end
