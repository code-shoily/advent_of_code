defmodule AdventOfCode.Algorithms.Geometry do
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
end
