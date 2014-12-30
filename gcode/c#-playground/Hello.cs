using System.IO;
using MimeKit;
class Hello
{
    static void Main( )
    {
        // Use the system console object
            FileStream stream = new FileStream("/home/bhj/Maildir/SentMails/cur/1419597900.M517484P16503Q0.bhj-home:2,S", FileMode.Open);
            var parser = new MimeParser (stream, MimeFormat.Entity);
            var message = parser.ParseMessage ();

            FileStream output = new FileStream("/home/bhj/tmp/out.txt", FileMode.Create);
            foreach (var part in message.BodyParts) {
                    part.ContentObject.DecodeTo (output);
                    // do something
            }
    }
}
