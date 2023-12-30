defmodule AdventOfCode.Y2023.Day19Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2319

  alias AdventOfCode.Y2023.Day19, as: Solution

  test "Year 2023, Day 19 run/1" do
    assert Solution.run() == {432_427, 143_760_172_569_135}
  end
end
