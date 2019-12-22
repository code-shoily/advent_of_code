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
    process()
    |> the_200th_asteroid()
    |> result()
  end

  defp result({x, y}), do: x * 100 + y

  defp locations_with_visible_asteroids(world) do
    world
    |> Enum.filter(fn {_, type} -> type == @asteroid end)
    |> Enum.map(fn {position, _} ->
      {position, visible_asteroids(world, position)}
    end)
  end

  defp visible_asteroids(world, source) do
    world
    |> asteroids_around(source)
    |> Enum.map(fn {_, asteroids} ->
      Enum.min_by(asteroids, &distance(&1, source))
    end)
  end

  def the_200th_asteroid(world) do
    position =
      world
      |> locations_with_visible_asteroids()
      |> Enum.max_by(fn {_, asteroids} -> length(asteroids) end)
      |> elem(0)

    nth_android(world, position, 200)
  end

  def nth_android(world, position, n) do
    nth_android(world, position, n, [])
  end

  defp nth_android(world, position, n, [] = _) do
    next =
      world
      |> visible_asteroids(position)
      |> radial_sort(position)

    nth_android(world, position, n, next)
  end

  defp nth_android(_, _, 1, [target | _]), do: target

  defp nth_android(world, position, n, [target | rest]) do
    world
    |> Map.put(target, ".")
    |> nth_android(position, n - 1, rest)
  end

  defp distance({x1, y1}, {x2, y2}),
    do: :math.sqrt(:math.pow(x1 - x2, 2) + :math.pow(y1 - y2, 2))

  defp radial_sort(objects, {xi, yi}) do
    objects
    |> Enum.sort_by(fn {x, y} ->
      case :math.atan2(x - xi, yi - y) * 180 / :math.pi() do
        θ when θ < 0 -> 360 + θ
        θ -> θ
      end
    end)
  end

  def run do
    %{
      problem_1: run_1(),
      problem_2: run_2()
    }
  end
end
