defmodule AdventOfCode.Y2019.Day10 do
  use AdventOfCode.Data.InputReader, year: 2019, day: 10

  @asteroid "#"

  def process() do
    input!()
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, row} ->
      line
      |> String.codepoints()
      |> Enum.with_index()
      |> Enum.map(fn {maybe_asteroid, col} ->
        {{col, row}, maybe_asteroid}
      end)
    end)
    |> Map.new()
  end

  def run_1 do
    process()
    |> visible_asteroid_count()
    |> Enum.max_by(fn {_, number} -> number end)
    |> elem(1)
  end

  defp visible_asteroid_count(world) do
    world
    |> Enum.filter(fn {_, object} -> object == @asteroid end)
    |> Enum.map(fn {position, _} ->
      {position, world |> visible_asteroids_from(position) |> length()}
    end)
  end

  defp asteroids_around(world, source) do
    world
    |> Enum.filter(fn {position, object} -> object == @asteroid and position != source end)
    |> Enum.group_by(fn {position, _} -> radian_of(position, source) end, &elem(&1, 0))
  end

  defp visible_asteroids_from(world, source) do
    world
    |> asteroids_around(source)
    |> Enum.map(fn {_, asteroids} -> hd(asteroids) end)
  end

  defp radian_of({x1, y1}, {x2, y2}), do: :math.atan2(y1 - y2, x1 - x2)

  def run_2 do
    1
  end

  def run do
    %{
      problem_1: run_1(),
      problem_2: run_2()
    }
  end
end
