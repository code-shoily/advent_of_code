defmodule AdventOfCode.Y2018.Day6 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2018/day/6
  """
  use AdventOfCode.Data.InputReader, year: 2018, day: 6

  @type point :: {integer(), integer()}
  @type corners :: {point, point}

  def process(data \\ input!()) do
    data
    |> String.split("\n")
    |> Enum.map(
      &(&1
        |> String.split(",")
        |> Enum.map(fn n -> n |> String.trim() |> String.to_integer() end)
        |> List.to_tuple())
    )
  end

  def manhattan_distance({x1, y1}, {x2, y2}), do: abs(x2 - x1) + abs(y2 - y1)

  @spec get_corners([point]) :: corners
  def get_corners(points) do
    {{xmin, _}, {xmax, _}} = Enum.min_max_by(points, &elem(&1, 0))
    {{_, ymin}, {_, ymax}} = Enum.min_max_by(points, &elem(&1, 1))

    {{xmin, ymin}, {xmax, ymax}}
  end

  @spec create_world(corners) :: %{required(point) => integer()}
  def create_world({{xl, yt}, {xr, yb}}) do
    for x <- xl..xr, y <- yt..yb, into: %{} do
      {{x, y}, 0}
    end
  end

  def remove_edges(points, {{xl, yt}, {xr, yb}}) do
    extremes = MapSet.new([xl, xr, yt, yb])

    points
    |> Enum.reject(fn {x, y} ->
      x in extremes or y in extremes
    end)
  end

  defp label_points(points) do
    points
    |> Enum.with_index(1)
    |> Enum.into(%{})
  end

  defp create_cache(labeled_points) do
    labeled_points
    |> Map.values()
    |> Enum.map(&{&1, 0})
    |> Enum.into(%{})
  end

  defp initialize_universe() do
    points = process()
    corners = get_corners(points)
    world = create_world(corners)

    finite_points = remove_edges(points, corners)
    labels = label_points(finite_points)
    cache = create_cache(labels)

    %{
      points: finite_points,
      world: world,
      labels: labels,
      cache: cache
    }
  end

  def get_nearest_area(areas) do
    areas
    |> Enum.sort_by(fn {_, distance} -> distance end)
    |> Enum.take(2)
    |> (fn
          [{_, d}, {_, d}] -> 0
          [{p, _} | _] -> p
        end).()
  end

  def run_1 do
    %{points: _, world: _, labels: _, cache: cache} = initialize_universe()

    Enum.max_by(cache, fn {_, v} -> v end)
  end

  def run_2 do
    {:not_implemented, 2}
  end

  def run, do: {run_1(), run_2()}
end
