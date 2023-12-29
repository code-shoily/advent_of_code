defmodule AdventOfCode.Y2023.Day24Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2324

  alias AdventOfCode.Y2023.Day24, as: Solution

  test "Year 2023, Day 24 run/1" do
    assert Solution.run() == {13_965, 578_177_720_733_043}
  end
end
