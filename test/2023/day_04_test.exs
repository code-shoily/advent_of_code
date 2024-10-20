defmodule AdventOfCode.Y2023.Day04Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2304

  alias AdventOfCode.Y2023.Day04, as: Solution

  test "Year 2023, Day 4 run/1" do
    assert Solution.run() == {24_542, 8_736_438}
  end
end
