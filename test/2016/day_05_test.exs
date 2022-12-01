defmodule AdventOfCode.Y2016.Day05Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y1605_slow

  alias AdventOfCode.Y2016.Day05, as: Solution

  @tag :skip_slow
  test "Year 2016, Day 5" do
    assert Solution.run() == {"f77a0e6e", "999828ec"}
  end
end
