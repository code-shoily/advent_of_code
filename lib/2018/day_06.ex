defmodule AdventOfCode.Y2018.Day06 do
  @moduledoc """
  --- Day 6: Chronal Coordinates ---
  Problem Link: https://adventofcode.com/2018/day/6
  Difficulty: s
  Tags: grid geometry optimization parallel
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2018, 6)

  def run(input \\ input()) do
    points = parse(input)
    # Combine both parts in one scan
    solve(points, 10_000)
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      Regex.scan(~r/\d+/, line)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
  end

  def covers_most_points(points) do
    solve(points, -1) |> elem(0)
  end

  def covers_distances_within(points, threshold) do
    solve(points, threshold) |> elem(1)
  end

  def solve(points, threshold) do
    {{x1, x2}, {y1, y2}} = get_corners(points)

    aggregated =
      y1..y2
      |> Task.async_stream(
        fn y ->
          Enum.reduce(x1..x2, {%{}, MapSet.new(), 0}, fn x, {counts, infinites, p2_cnt} ->
            {nearest, d1, d2, total_d} = find_nearest_and_sum(x, y, points)

            p2_inc = if total_d < threshold, do: 1, else: 0

            if d1 < d2 do
              new_counts = Map.update(counts, nearest, 1, &(&1 + 1))

              new_infinites =
                if x == x1 or x == x2 or y == y1 or y == y2,
                  do: MapSet.put(infinites, nearest),
                  else: infinites

              {new_counts, new_infinites, p2_cnt + p2_inc}
            else
              {counts, infinites, p2_cnt + p2_inc}
            end
          end)
        end,
        timeout: :infinity
      )
      |> Enum.reduce({%{}, MapSet.new(), 0}, fn {:ok, {c, i, p2}}, {acc_c, acc_i, acc_p2} ->
        {Map.merge(acc_c, c, fn _, v1, v2 -> v1 + v2 end), MapSet.union(acc_i, i), acc_p2 + p2}
      end)

    {counts, infinite_points, p2_count} = aggregated

    p1_max =
      counts
      |> Enum.reject(fn {p, _} -> MapSet.member?(infinite_points, p) end)
      |> Enum.map(&elem(&1, 1))
      |> Enum.max(fn -> 0 end)

    {p1_max, p2_count}
  end

  def get_corners(points) do
    {{min_x, max_x}, {min_y, max_y}} =
      points
      |> Enum.unzip()
      |> then(fn {xs, ys} -> {Enum.min_max(xs), Enum.min_max(ys)} end)

    {{min_x, max_x}, {min_y, max_y}}
  end

  defp find_nearest_and_sum(x, y, points) do
    Enum.reduce(points, {nil, :infinity, :infinity, 0}, fn {px, py} = p, {b_p, b_d, s_d, sum} ->
      d = abs(x - px) + abs(y - py)

      cond do
        d < b_d -> {p, d, b_d, sum + d}
        d < s_d -> {b_p, b_d, d, sum + d}
        true -> {b_p, b_d, s_d, sum + d}
      end
    end)
  end
end
