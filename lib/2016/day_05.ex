defmodule AdventOfCode.Y2016.Day05 do
  @moduledoc """
  --- Day 5: How About a Nice Game of Chess? ---
  Problem Link: https://adventofcode.com/2016/day/5
  !FIXME: This is slow!
  """
  @input "cxdnnyjw"

  def run(input \\ @input) do
    solution_1 = Task.async(fn -> run_1(input) end)
    solution_2 = Task.async(fn -> run_2(input) end)

    {
      Task.await(solution_1, :infinity),
      Task.await(solution_2, :infinity)
    }
  end

  def run_1(input) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.reduce_while({"", 0}, fn x, {hash, iter} ->
      new_hash = md5(input <> to_string(x))

      case {five_zeroes?(new_hash), iter + 1} do
        {true, 8} -> {:halt, hash <> String.at(new_hash, 5)}
        {true, val} -> {:cont, {hash <> String.at(new_hash, 5), val}}
        _ -> {:cont, {hash, iter}}
      end
    end)
  end

  @valid "01234567"
  def run_2(input) do
    Stream.iterate(1, &(&1 + 1))
    |> Enum.reduce_while(%{coords: [], done: MapSet.new()}, fn x,
                                                               %{coords: coords, done: done} = acc ->
      hash = md5(input <> to_string(x))
      {position, value} = {String.at(hash, 5), String.at(hash, 6)}
      valid? = String.contains?(@valid, position) and position not in done

      case {five_zeroes?(hash), valid?} do
        {true, true} ->
          done = MapSet.put(done, position)
          coords = [{position, value} | coords]
          instruction = (length(coords) == 8 && :halt) || :cont
          {instruction, %{coords: coords, done: done}}

        _ ->
          {:cont, acc}
      end
    end)
    |> construct_pasword()
  end

  defp md5(door_id) do
    :crypto.hash(:md5, door_id) |> Base.encode16(case: :lower)
  end

  defp five_zeroes?(door_id), do: String.starts_with?(door_id, "00000")

  defp construct_pasword(%{coords: coords}) do
    coords
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map_join(fn {_, v} -> v end)
  end
end
