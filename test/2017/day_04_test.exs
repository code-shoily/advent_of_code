defmodule AdventOfCode.Y2017.Day04Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1704

  alias AdventOfCode.Y2017.Day04, as: Solution

  test "containes_duplicates returns true if passphrases contain duplicates" do
    assert Solution.duplicates?(~w/aa bb cc dd ee/)
    refute Solution.duplicates?(~w/aa bb cc dd aa/)
    assert Solution.duplicates?(~w/aa bb cc dd aaa/)
  end

  test "containes_duplicates returns true if passphrases contain anagrams" do
    assert Solution.anagrams?(~w/abcde fghij/)
    assert Solution.anagrams?(~w/a ab abc abd abf abj/)
    assert Solution.anagrams?(~w/iiii oiii ooii oooi oooo/)
    refute Solution.anagrams?(~w/oiii ioii iioi iiio/)
  end

  test "Year 2017, Day 4" do
    assert Solution.run() == {455, 186}
  end
end
