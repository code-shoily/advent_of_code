defmodule AdventOfCode.Y2015.Day04 do
  @moduledoc """
  --- Day 4: The Ideal Stocking Stuffer ---
  Problem Link: https://adventofcode.com/2015/day/4
  """
  def run_1, do: lowest_number("bgvyzdsv", 1)
  def run_2, do: "bgvyzdsv" |> lowest_number_2(1)

  defp h(s, n), do: Base.encode16(:crypto.hash(:md5, s <> to_string(n)))
  defp satisfy(_, "0" <> "0" <> "0" <> "0" <> "0" <> _, n), do: n
  defp satisfy(s, _, n), do: lowest_number(s, n + 1)
  def lowest_number(s, number), do: satisfy(s, h(s, number), number)

  defp satisfy_2(_, "0" <> "0" <> "0" <> "0" <> "0" <> "0" <> _, n), do: n
  defp satisfy_2(s, _, n), do: lowest_number_2(s, n + 1)
  def lowest_number_2(s, number), do: satisfy_2(s, h(s, number), number)
end
