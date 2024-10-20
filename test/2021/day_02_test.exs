defmodule AdventOfCode.Y2021.Day02Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2102

  alias AdventOfCode.Y2021.Day02, as: Solution

  test "Year 2021, Day 2" do
    assert Solution.run() == {1_660_158, 1_604_592_846}
  end
end
