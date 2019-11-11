defmodule AdventOfCode.Data.InputReader do
  @moduledoc """
  Reads input from file
  """
  @type year :: integer()
  @type day :: integer()

  defmacro __using__(year: year, day: day) do
    quote do
      @spec build_path(year(), day()) :: String.t()
      defp build_path(year, day), do: "#{File.cwd!()}/lib/data/inputs/#{year}_#{day}.txt"

      @spec read!(year(), day()) :: binary()
      def read!(year, day) do
        year |> build_path(day) |> File.read!()
      end

      @spect input!() :: binary()
      def input!, do: read!(unquote(year), unquote(day))
    end
  end
end
