defmodule AdventOfCode.Y2022.Day13Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2213

  alias AdventOfCode.Y2022.Day13, as: Solution

  test "Year 2022, Day 13 run/1" do
    assert Solution.run() == {5503, 20952}
  end
end
