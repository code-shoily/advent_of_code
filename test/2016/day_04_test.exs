defmodule AdventOfCode.Y2016.Day04Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1604

  alias AdventOfCode.Y2016.Day04, as: Solution

  test "Year 2016, Day 4" do
    assert Solution.run() == {158_835, 993}
  end
end
