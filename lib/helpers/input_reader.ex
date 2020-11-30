defmodule AdventOfCode.Helpers.InputReader do
  @moduledoc """
  Reads input from file
  """
  @input_dir Path.join(:code.priv_dir(:advent_of_code), "input_files")

  defmacro __using__(year: year, day: day) do
    quote do
      defp build_path(year, day), do: "#{unquote(@input_dir)}/#{year}_#{day}.txt"

      def read!(year, day) do
        year |> build_path(day) |> File.read!()
      end

      def input!, do: read!(unquote(year), unquote(day)) |> String.trim()
    end
  end
end
