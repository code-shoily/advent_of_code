defmodule AdventOfCode.Y2022.Day20Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2220

  alias AdventOfCode.Y2022.Day20, as: Solution

  test "Year 2022, Day 20 run/1" do
    assert Solution.run() == {7228, 4_526_232_706_281}
  end
end
