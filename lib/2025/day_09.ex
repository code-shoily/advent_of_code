defmodule AdventOfCode.Y2025.Day09 do
  @moduledoc """
  --- Day 9: Movie Theater ---
  Problem Link: https://adventofcode.com/2025/day/9
  Difficulty: m
  Tags: geometry2d polygon
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2025, 9)

  def run(input \\ input()) do
    red_tiles = parse(input)

    xs = red_tiles |> Enum.map(fn {x, _} -> x end) |> Enum.uniq() |> Enum.sort()
    ys = red_tiles |> Enum.map(fn {_, y} -> y end) |> Enum.uniq() |> Enum.sort()

    x_map = xs |> Enum.with_index() |> Map.new()
    y_map = ys |> Enum.with_index() |> Map.new()
    num_x = length(xs) - 1
    num_y = length(ys) - 1

    segments =
      red_tiles
      |> Enum.chunk_every(2, 1, [List.first(red_tiles)])
      |> Enum.map(fn [p1, p2] -> {p1, p2} end)

    xs_t = List.to_tuple(xs)
    ys_t = List.to_tuple(ys)

    inside_cells =
      for i <- 0..(num_x - 1),
          x_low = elem(xs_t, i),
          x_high = elem(xs_t, i + 1),
          mid_x = (x_low + x_high) / 2,
          j <- 0..(num_y - 1),
          y_low = elem(ys_t, j),
          y_high = elem(ys_t, j + 1),
          mid_y = (y_low + y_high) / 2,
          point_in_polygon?({mid_x, mid_y}, segments),
          do: {i, j}

    prefix_sum = build_prefix_sum(inside_cells, num_x, num_y)

    {solve_1(red_tiles), solve_2(red_tiles, x_map, y_map, prefix_sum)}
  end

  defp solve_1(red_tiles) do
    for(p1 <- red_tiles, p2 <- red_tiles, do: area(p1, p2)) |> Enum.max()
  end

  defp solve_2(red_tiles, x_map, y_map, prefix_sum) do
    max_a =
      for p1 <- red_tiles,
          p2 <- red_tiles do
        u = Map.get(x_map, elem(p1, 0))
        w = Map.get(x_map, elem(p2, 0))
        v = Map.get(y_map, elem(p1, 1))
        z = Map.get(y_map, elem(p2, 1))

        {u, w} = if u < w, do: {u, w}, else: {w, u}
        {v, z} = if v < z, do: {v, z}, else: {z, v}

        if all_inside?(u, v, w, z, prefix_sum) do
          area(p1, p2)
        else
          0
        end
      end
      |> Enum.max()

    max_a
  end

  defp area({x1, y1}, {x2, y2}) do
    (abs(x1 - x2) + 1) * (abs(y1 - y2) + 1)
  end

  defp point_in_polygon?({mx, my}, segments) do
    Enum.count(segments, fn {{x1, y1}, {x2, y2}} ->
      if y1 == y2 and y1 > my do
        min_x = min(x1, x2)
        max_x = max(x1, x2)
        mx >= min_x and mx < max_x
      else
        false
      end
    end)
    |> Integer.mod(2) == 1
  end

  defp build_prefix_sum(inside_cells, num_x, num_y) do
    inside_set = MapSet.new(inside_cells)

    Enum.reduce(0..(num_x - 1), %{}, fn i, acc_i ->
      Enum.reduce(0..(num_y - 1), acc_i, fn j, acc_j ->
        val = if MapSet.member?(inside_set, {i, j}), do: 1, else: 0

        sum =
          val +
            Map.get(acc_j, {i - 1, j}, 0) +
            Map.get(acc_j, {i, j - 1}, 0) -
            Map.get(acc_j, {i - 1, j - 1}, 0)

        Map.put(acc_j, {i, j}, sum)
      end)
    end)
  end

  defp all_inside?(u, v, w, z, prefix_sum) do
    if u == w or v == z do
      true
    else
      expected = (w - u) * (z - v)

      actual =
        Map.get(prefix_sum, {w - 1, z - 1}, 0) -
          Map.get(prefix_sum, {u - 1, z - 1}, 0) -
          Map.get(prefix_sum, {w - 1, v - 1}, 0) +
          Map.get(prefix_sum, {u - 1, v - 1}, 0)

      actual == expected
    end
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [x, y] = String.split(line, ",")
      {String.to_integer(x), String.to_integer(y)}
    end)
  end
end
