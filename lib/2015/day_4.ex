defmodule AdventOfCode.Y2015.Day4 do
  defp h(s, number),
    do:
      :crypto.hash(:md5, s <> Integer.to_string(number))
      |> Base.encode16()

  defp satisfy(_, "0" <> "0" <> "0" <> "0" <> "0" <> _, n), do: n
  defp satisfy(s, _, n), do: lowest_number(s, n + 1)

  def lowest_number(s, number), do: satisfy(s, h(s, number), number)

  def run, do: "bgvyzdsv" |> lowest_number(1)
end
