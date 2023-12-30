defmodule AdventOfCode.Y2023.Day22 do
  @moduledoc """
  --- Day 22: Sand Slabs ---
  Problem Link: https://adventofcode.com/2023/day/22
  Difficulty: xl
  Tags: stack range not-fast-enough
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 22)

  def run(input \\ input()) do
    input
    |> parse()
    |> supports()
    |> then(&{length(free(&1)), disintegrate(&1)})
  end

  def parse(data \\ input()) do
    for {line, i} <- Enum.with_index(Transformers.lines(data)) do
      line
      |> String.split([",", "~"], trim: true)
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(3)
      |> Enum.zip_with(fn [a, b] -> Range.new(min(a, b), max(a, b)) end)
      |> List.to_tuple()
      |> then(fn {x, y, z} -> {to_alpha(i), x, y, z} end)
    end
  end

  defp supports(data), do: data |> drop() |> support()
  defp to_alpha(i), do: to_alpha(i, [])
  defp to_alpha(i, acc) when i < 26, do: to_string([rem(i, 26) + 65 | acc])
  defp to_alpha(i, acc), do: to_alpha(div(i, 26) - 1, [rem(i, 26) + 65 | acc])

  defp drop(bricks),
    do:
      bricks
      |> Enum.sort_by(fn {_, _, _, z1.._} -> z1 end)
      |> drop([])

  defp drop([], settled), do: settled

  defp drop([{_, _, _, 1.._} = brick | bricks], settled),
    do: drop(bricks, [brick | settled])

  defp drop([{id, ax, ay, az} | bricks], settled) do
    {_, _, _, _..bz2} =
      Enum.filter(settled, fn {_, bx, by, _} ->
        not (Range.disjoint?(ax, bx) or
               Range.disjoint?(ay, by))
      end)
      |> Enum.max_by(
        fn {_, _, _, _..bz2} -> bz2 end,
        fn -> {nil, nil, nil, 0..0} end
      )

    az1 = bz2 + 1
    az2 = bz2 + Range.size(az)
    brick = {id, ax, ay, az1..az2}

    drop(bricks, [brick | settled])
  end

  defp support(bricks), do: support(bricks, bricks, %{})
  defp support([], _, supports), do: supports

  defp support([{_, ax, ay, az1.._} = current | rest], bricks, supports) do
    others =
      Enum.filter(bricks, fn {_, bx, by, _..bz2} ->
        bz2 == az1 - 1 and
          not (Range.disjoint?(ax, bx) or
                 Range.disjoint?(ay, by))
      end)

    supports =
      Map.update(
        supports,
        current,
        %{is_above: others, is_below: []},
        fn %{is_above: above} = existing ->
          %{existing | is_above: others ++ above}
        end
      )

    supports =
      Enum.reduce(others, supports, fn other, supports ->
        Map.update(
          supports,
          other,
          %{is_above: [], is_below: [current]},
          fn %{is_below: below} = existing ->
            %{existing | is_below: [current | below]}
          end
        )
      end)

    support(rest, bricks, supports)
  end

  defp free(supports) do
    supports
    |> Enum.filter(fn {_, %{is_above: is_above}} -> length(is_above) == 1 end)
    |> Enum.flat_map(fn {_, %{is_above: is_above}} -> is_above end)
    |> MapSet.new()
    |> then(fn required ->
      supports
      |> Map.keys()
      |> MapSet.new()
      |> MapSet.difference(required)
      |> Enum.to_list()
    end)
  end

  defp chain(brick, supports), do: chain([brick], supports, MapSet.new())
  defp chain([], _, acc), do: acc

  defp chain([brick | rest], supports, acc) do
    destroyed =
      supports[brick].is_below
      |> Enum.filter(fn above_brick ->
        below_bricks = supports[above_brick].is_above

        length(below_bricks) == 1 or
          Enum.all?(below_bricks, fn below -> MapSet.member?(acc, below) end)
      end)

    destroyed
    |> MapSet.new()
    |> MapSet.union(acc)
    |> then(fn acc -> chain(rest ++ destroyed, supports, acc) end)
  end

  defp disintegrate(supports) do
    for brick <- Map.keys(supports), reduce: 0 do
      acc ->
        brick
        |> chain(supports)
        |> MapSet.size()
        |> Kernel.+(acc)
    end
  end
end
