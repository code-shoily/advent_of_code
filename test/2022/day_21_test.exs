defmodule AdventOfCode.Y2022.Day21Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2221

  alias AdventOfCode.Y2022.Day21, as: Solution

  test "Year 2022, Day 21 run/1" do
    assert Solution.run() == {282_285_213_953_670, 3_699_945_358_564}
  end
end
