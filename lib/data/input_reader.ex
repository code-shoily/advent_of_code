defmodule AdventOfCode.Data.InputReader do
  @moduledoc """
  Reads input from file
  """
  defmacro __using__(year: year, day: day) do
    quote do
      defp build_path(year, day), do: "#{File.cwd!()}/lib/data/inputs/#{year}_#{day}.txt"

      def read!(year, day) do
        year |> build_path(day) |> File.read!()
      end

      def input!, do: read!(unquote(year), unquote(day))
    end
  end
end
