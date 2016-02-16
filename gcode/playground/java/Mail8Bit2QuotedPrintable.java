import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Properties;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.Session;
import java.io.ByteArrayOutputStream;
import javax.mail.internet.MimeUtility;
import java.io.OutputStream;
import java.io.InputStream;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class Mail8Bit2QuotedPrintable {

    private static String getCharset(String contentType) {
        Pattern charSetPattern = Pattern.compile("chareset\\s*=\\s*['\"]?(.*?)['\"]?", Pattern.CASE_INSENSITIVE);
        Matcher charSetMatcher = charSetPattern.matcher(contentType);
        String charSet = "UTF-8";
        if (charSetMatcher.find()) {
            charSet = charSetMatcher.group(1);
        }

        return charSet;
    }

    public static void main(String[] args) {

        try {
            String msgfile = args[0];
            String outfile = args[1];

            Session session = Session.getInstance(new Properties());
            MimeMessage mm = new MimeMessage(session, new FileInputStream(msgfile));
            // assuming we know that it's a multipart; otherwise, check Content-Type first...
            if (mm.isMimeType("text/*")) {
                if (mm.getEncoding().equals("8bit")) {
                    InputStream input = mm.getInputStream();
                    byte[] bArray = new byte[mm.getSize()];
                    if (input.read(bArray) != bArray.length) {
                        throw new Exception("error read");
                    }
                    String contentType = mm.getHeader("Content-Type", ", ");
                    String charSet = getCharset(contentType);

                    String text = new String(bArray, charSet);
                    mm.setContent(text, mm.getContentType());
                    //String text = new String(bArray, mm.)

                    mm.setHeader("Content-Transfer-Encoding", "quoted-printable");
                }
            } else if (mm.isMimeType("multipart/alternative")) {
                MimeMultipart multi = (MimeMultipart) mm.getContent();
                for (int i = 0; i < multi.getCount(); i++) {
                    MimeBodyPart mbp = (MimeBodyPart) multi.getBodyPart(i);
                    if (!mbp.isMimeType("text/*")) {
                        continue;
                    }

                    InputStream mbpInput = mbp.getInputStream();
                    byte[] bArray = new byte[mbp.getSize()];
                    if (mbpInput.read(bArray) != bArray.length) {
                        throw new Exception("error read");
                    }

                    String contentType = mbp.getHeader("Content-Type", ", ");
                    String charSet = getCharset(contentType);

                    String text = new String(bArray, charSet);
                    mbp.setContent(text, mbp.getContentType());
                    //String text = new String(bArray, mbp.)

                    mbp.setHeader("Content-Transfer-Encoding", "quoted-printable");
                }
            }
            mm.saveChanges();
            mm.writeTo(new FileOutputStream(outfile));
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }

    }
}
