defmodule AdventOfCode.Y2018.Day06 do
  @moduledoc """
  --- Day 6: Chronal Coordinates ---
  Problem Link: https://adventofcode.com/2018/day/6
  Difficulty: s
  Tags: not-fast-enough grid measurement
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  import AdventOfCode.Algorithms.Geometry, only: [manhattan_distance: 2]

  @type point :: {integer(), integer()}
  @type points :: list(point())
  @type corners :: {point(), point()}
  @type world :: list({point(), list({point(), integer()})})

  def input, do: InputReader.read_from_file(2018, 6)

  def run(input \\ input()) do
    input = parse(input)

    {covers_most_points(input), covers_distances_within(input, 10_000)}
  end

  @spec parse(binary()) :: points()
  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> Transformers.words(",")
      |> Enum.map(&String.to_integer(String.trim(&1)))
      |> List.to_tuple()
    end)
  end

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
    |> Enum.sum_by(&elem(&1, 1))
    |> case do
      n when n < threshold -> 1
      _ -> 0
    end
  end
end
