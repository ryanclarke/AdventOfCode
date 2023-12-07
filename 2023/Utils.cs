public static partial class Utils
{
  public static T ID<T>(this T input) => input;

  [GeneratedRegex(" +")]
  public static partial Regex MultipleWhitespaceRegex();
  public static string RemoveMultipleWhitespace(this string s) => MultipleWhitespaceRegex().Replace(s, " ");

}