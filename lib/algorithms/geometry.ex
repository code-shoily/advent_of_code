defmodule AdventOfCode.Algorithms.Geometry do
  @moduledoc """
  Geometry related functions that mostly have to do with geometric computations and measurements.
  """
  @type point2d :: {integer(), integer()}

  @doc """
  Following the shoelace formula, computes the area of polygon.

  ## Examples

      iex> points = [{0, 1}, {2, 3}, {4, 7}, {0, 1}]
      iex> Geometry.polygon_area(points)
      2.0

      iex> points = [{0, 0}, {4, 0}, {4, 4}, {0, 4}, {0, 0}]
      iex> Geometry.polygon_area(points)
      16.0

      iex> points = [{0, 0}, {4, 0}, {2, 4}, {0, 0}]
      iex> Geometry.polygon_area(points)
      8.0

  """
  @spec polygon_area(list(point2d())) :: number()
  def polygon_area([first | points]) do
    points
    |> Enum.reduce({first, 0.0}, fn {x, y} = p, {{xi, yi}, area} ->
      {p, area + (x - xi) * (y + yi)}
    end)
    |> then(fn {_, area} -> abs(area / 2) end)
  end

  @doc """
  Computes manhattan distance between two points

  ## Example

      iex> Geometry.manhattan_distance({0, 0}, {10, 20})
      30

      iex> Geometry.manhattan_distance({100, 50}, {10, 10})
      130

      iex> Geometry.manhattan_distance({15, 17}, {16, 19})
      3
  """
  @spec manhattan_distance(point2d, point2d) :: integer()
  def manhattan_distance({x1, y1}, {x2, y2}), do: abs(x2 - x1) + abs(y2 - y1)
end
