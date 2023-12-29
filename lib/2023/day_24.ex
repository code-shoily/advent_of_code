defmodule AdventOfCode.Y2023.Day24 do
  @moduledoc """
  --- Day 24: Never Tell Me The Odds ---
  Problem Link: https://adventofcode.com/2023/day/24
  Difficulty: xxl
  Tags: geometry2d refactor
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @min 200_000_000_000_000
  @max 400_000_000_000_000

  def input, do: InputReader.read_from_file(2023, 24)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def parse(input \\ input()) do
    for line <- Transformers.lines(input) do
      for parts <- String.split(line, " @ ") do
        parts
        |> String.split([",", " "], trim: true)
        |> Enum.map(&String.to_integer/1)
        |> List.to_tuple()
      end
      |> List.to_tuple()
    end
  end

  defp run_1(input) do
    input
    |> Enum.map(fn {{px, py, _}, {vx, vy, _}} -> {{px, py}, {vx, vy}} end)
    |> intersections()
    |> Enum.count(fn
      {{x, y}, {{x1, y1}, {vx1, vy1}}, {{x2, y2}, {vx2, vy2}}}
      when @min <= x and x <= @max and @min <= y and y <= @max and
             (x - x1) * vx1 > 0 and
             (y - y1) * vy1 > 0 and
             (x - x2) * vx2 > 0 and
             (y - y2) * vy2 > 0 ->
        true

      _ ->
        false
    end)
  end

  defp run_2([{h0_pos, h0_dir}, h2, h3, h4 | _]) do
    [{h0_pos, h0_dir}, h2, h3, h4]
    |> Enum.map(fn {pos, dir} -> {sub(pos, h0_pos), sub(dir, h0_dir)} end)
    |> then(fn [_, {h1_pos, h1_dir}, h2, h3 | _] ->
      h1_pos
      |> add(h1_dir)
      |> cross(h1_pos)
      |> then(fn n ->
        {p2, t2} = plane_and_line_intersection({0, 0, 0}, n, h2)
        {p3, t3} = plane_and_line_intersection({0, 0, 0}, n, h3)
        dir = vec_div(sub(p2, p3), t2 - t3)
        pos = sub(p2, mul(dir, t2))
        {x, y, z} = add(pos, h0_pos)
        x + y + z
      end)
    end)
  end

  defp intersections(list), do: intersections(list, [])
  defp intersections([], acc), do: acc
  defp intersections([a | rest], acc), do: intersections(rest, intersections(rest, a, acc))
  defp intersections([], _, acc), do: acc
  defp intersections([b | rest], a, acc), do: intersections(rest, a, [intersection(a, b) | acc])

  defp intersection(e1, e2) do
    case {line_equation(e1), line_equation(e2)} do
      {{a, _}, {b, _}} when a - b == 0 ->
        nil

      {{a, c}, {b, d}} ->
        x = (d - c) / (a - b)
        y = a * x + c
        {{x, y}, e1, e2}
    end
  end

  def line_equation({{px, py}, {vx, vy}}) do
    slope = vy / vx
    {slope, -slope * px + py}
  end

  defp plane_and_line_intersection(p0, n, {pos, dir}) do
    a = dot(sub(p0, pos), n)
    b = dot(dir, n)
    t = div(a, b)
    p = add_prod(pos, dir, t)
    {p, t}
  end

  defp add_prod({v10, v11, v12}, {v20, v21, v22}, s),
    do: {s * v20 + v10, s * v21 + v11, s * v22 + v12}

  defp cross({v10, v11, v12}, {v20, v21, v22}),
    do: {v11 * v22 - v12 * v21, v12 * v20 - v10 * v22, v10 * v21 - v11 * v20}

  defp dot({v10, v11, v12}, {v20, v21, v22}), do: v10 * v20 + v11 * v21 + v12 * v22
  defp add({v10, v11, v12}, {v20, v21, v22}), do: {v10 + v20, v11 + v21, v12 + v22}
  defp sub({v10, v11, v12}, {v20, v21, v22}), do: {v10 - v20, v11 - v21, v12 - v22}
  defp mul({v10, v11, v12}, s), do: {v10 * s, v11 * s, v12 * s}
  defp vec_div({v10, v11, v12}, s), do: {div(v10, s), div(v11, s), div(v12, s)}
end
