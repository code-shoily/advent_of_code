defmodule AdventOfCode.Y2018.Day06 do
  @moduledoc """
  --- Day 6: Chronal Coordinates ---
  Problem Link: https://adventofcode.com/2018/day/6
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 6

  @type point :: {integer(), integer()}
  @type points :: list(point())
  @type corners :: {point(), point()}
  @type world :: list({point(), list({point(), integer()})})

  def run_1 do
    input!()
    |> parse()
    |> covers_most_points()
  end

  def run_2 do
    input!()
    |> parse()
    |> covers_distances_within(10_000)
  end

  @spec parse(binary()) :: points()
  def parse(data) do
    data
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(&String.to_integer(String.trim(&1)))
      |> List.to_tuple()
    end)
  end

  @spec manhattan_distance(point, point) :: integer()
  def manhattan_distance({x1, y1}, {x2, y2}), do: abs(x2 - x1) + abs(y2 - y1)

  @spec get_corners([point]) :: corners
  def get_corners(points) do
    points
    |> Enum.unzip()
    |> Tuple.to_list()
    |> Enum.map(&Enum.min_max/1)
    |> List.to_tuple()
  end

  @spec create_world(points()) :: world()
  defp create_world(points) do
    {{xl, xr}, {yt, yb}} = get_corners(points)

    for x <- xr..xl, y <- yt..yb do
      {{x, y},
       points
       |> Enum.map(fn {xi, yi} ->
         {{xi, yi}, manhattan_distance({xi, yi}, {x, y})}
       end)}
    end
  end

  @spec nearest_point(list({point(), integer()})) :: nil | point()
  defp nearest_point(distances) do
    distances
    |> Enum.sort_by(&elem(&1, 1))
    |> Enum.take(2)
    |> then(fn
      [{_, d}, {_, d}] -> nil
      [{p, _} | _] -> p
    end)
  end

  defp edges(world, {{xl, xr}, {yt, yb}}) do
    for {{x, y}, p} <- world,
        x == xl or y == yt or x == xr or y == yb or is_nil(p),
        into: %MapSet{},
        do: p
  end

  @spec covers_most_points(points()) :: integer()
  def covers_most_points(points) do
    world = Enum.map(create_world(points), fn {p, ds} -> {p, nearest_point(ds)} end)
    rejects = edges(world, get_corners(points))

    world
    |> Enum.group_by(&elem(&1, 1))
    |> Enum.reduce(-1, fn {p, ds}, largest ->
      (p not in rejects && max(length(ds), largest)) || largest
    end)
  end

  @spec covers_distances_within(points(), integer()) :: integer()
  def covers_distances_within(points, threshold) do
    Enum.reduce(create_world(points), 0, fn {_, d}, acc ->
      within_distance_threshold(d, threshold) + acc
    end)
  end

  defp within_distance_threshold(distances, threshold) do
    distances
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
    |> case do
      n when n < threshold -> 1
      _ -> 0
    end
  end
end
