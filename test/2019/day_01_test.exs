defmodule AdventOfCode.Y2019.Day01Test do
  @moduledoc false
  use ExUnit.Case, async: true
  @moduletag :y1901

  alias AdventOfCode.Y2019.Day01, as: Solution

  test "Year 2019, Day 1" do
    assert Solution.run() == {3_421_505, 5_129_386}
  end
end
