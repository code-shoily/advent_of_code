defmodule AdventOfCode.Y2023.Day22 do
  @moduledoc """
  --- Day 22: Sand Slabs ---
  Problem Link: https://adventofcode.com/2023/day/22
  Difficulty: xl
  Tags: range stack
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 22)

  def run(input \\ input()) do
    bricks = parse(input) |> Enum.sort_by(fn {_, _, _, z1.._//_} -> z1 end)

    # Settlement process using a heightmap
    # supports: map of brick_id -> list of ids it supports (is_below)
    # supported_by: map of brick_id -> list of ids that support it (is_above)
    {supports, supported_by, brick_ids} = settle(bricks)

    # Part 1: Count bricks that can be disintegrated safely
    # A brick is safe if every brick it supports has at least one other supporter.
    part1_count =
      brick_ids
      |> Enum.count(fn id ->
        up_bricks = Map.get(supports, id, [])

        Enum.all?(up_bricks, fn up_id ->
          length(Map.get(supported_by, up_id, [])) > 1
        end)
      end)

    # Part 2: Sum of all bricks that would fall for each disintegrated brick
    part2_sum =
      Enum.map(brick_ids, fn id ->
        count_falling_bricks(id, supports, supported_by)
      end)
      |> Enum.sum()

    {part1_count, part2_sum}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.map(fn {line, i} ->
      [x1, y1, z1, x2, y2, z2] =
        line
        |> String.split([",", "~"], trim: true)
        |> Enum.map(&String.to_integer/1)

      {i, x1..x2, y1..y2, z1..z2}
    end)
  end

  defp settle(bricks) do
    # height_map: 100-element tuple representing 10x10 grid
    # Each entry is {z_height, brick_id}
    # Index is x * 10 + y
    initial_hm = Tuple.duplicate({0, -1}, 100)

    {_, supports, supported_by, ids} =
      Enum.reduce(bricks, {initial_hm, %{}, %{}, []}, fn {id, x1..x2//_, y1..y2//_, z1..z2//_},
                                                         {hm, supports, supported_by, ids} ->
        # Find maximum height in the footprint
        max_z =
          Enum.reduce(x1..x2, 0, fn x, acc_x ->
            base = x * 10

            Enum.reduce(y1..y2, acc_x, fn y, acc_y ->
              max(acc_y, elem(hm, base + y) |> elem(0))
            end)
          end)

        # Find unique supporters at exactly max_z
        supporters =
          Enum.reduce(x1..x2, [], fn x, acc_x ->
            base = x * 10

            Enum.reduce(y1..y2, acc_x, fn y, acc_y ->
              case elem(hm, base + y) do
                {^max_z, sid} when sid != -1 -> [sid | acc_y]
                _ -> acc_y
              end
            end)
          end)
          |> Enum.uniq()

        # Update heightmap with new height and brick id
        new_z = max_z + Range.size(z1..z2)
        entry = {new_z, id}

        new_hm =
          Enum.reduce(x1..x2, hm, fn x, acc_x ->
            base = x * 10

            Enum.reduce(y1..y2, acc_x, fn y, acc_y ->
              put_elem(acc_y, base + y, entry)
            end)
          end)

        # Update relationships
        new_supported_by = Map.put(supported_by, id, supporters)

        new_supports =
          Enum.reduce(supporters, supports, fn sid, acc ->
            Map.update(acc, sid, [id], &[id | &1])
          end)

        {new_hm, new_supports, new_supported_by, [id | ids]}
      end)

    {supports, supported_by, ids}
  end

  defp count_falling_bricks(start_id, supports, supported_by) do
    in_degrees =
      Enum.reduce(supported_by, %{}, fn {id, supporters}, acc ->
        Map.put(acc, id, length(supporters))
      end)

    do_count_falling([start_id], supports, in_degrees, -1)
  end

  defp do_count_falling([], _supports, _in_degrees, count), do: count

  defp do_count_falling([id | rest], supports, in_degrees, count) do
    {next_ids, new_in_degrees} =
      Enum.reduce(Map.get(supports, id, []), {[], in_degrees}, fn next_id, {acc_ids, acc_deg} ->
        deg = acc_deg[next_id] - 1

        if deg == 0 do
          {[next_id | acc_ids], Map.put(acc_deg, next_id, 0)}
        else
          {acc_ids, Map.put(acc_deg, next_id, deg)}
        end
      end)

    do_count_falling(rest ++ next_ids, supports, new_in_degrees, count + 1)
  end
end
