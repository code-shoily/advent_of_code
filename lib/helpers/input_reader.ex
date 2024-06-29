defmodule AdventOfCode.Helpers.InputReader do
  @moduledoc """
  Reads input from file
  """
  @input_dir Path.join(:code.priv_dir(:advent_of_code), "input_files")

  @doc """
  Reads input from file. Reads from the input directory `priv/input_files` and sends
  a trimmed string if `trim` is true otherwise returns the data as raw. The reason `trim`
  is configurable because some AoC problems (i.e. Year 2017 day 19) has significant leading
  or trailing space(s)
  """
  @spec read_from_file(pos_integer(), pos_integer(), boolean()) :: binary()
  def read_from_file(year, day, trim \\ true) do
    case {trim, File.read!("#{@input_dir}/#{year}_#{day}.txt")} do
      {true, data} -> String.trim(data)
      {_, data} -> data
    end
  end
end
