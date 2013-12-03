import com.android.tools.idea.templates.*;
import freemarker.template.*;
import java.util.*;
import java.io.*;

public class FreeMarker {

    public static void main(String[] args) throws Exception {

        if (args.length < 2) {
            System.out.println(String.format("%s Error: usage: FreeMarker dir ftl key val ...%s", "FreeMarker.java:10:", ""));
        }


        /* ----------------------------------------------------------------------- */
        /* You should do this ONLY ONCE in the whole application life-cycle:       */

        /* Create and adjust the configuration */
        Configuration cfg = new Configuration();

        cfg.setDirectoryForTemplateLoading(new File(args[0]));
        cfg.setObjectWrapper(new DefaultObjectWrapper());
        cfg.setDefaultEncoding("UTF-8");
        cfg.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);

        /* ----------------------------------------------------------------------- */
        /* You usually do these for many times in the application life-cycle:      */

        /* Create a data-model */
        Map root = new HashMap();

        for (int i = 2; i < args.length; i += 2) {
            root.put(args[i], args[i+1]);
            if (args[i+1].equalsIgnoreCase("false!")) {
                root.put(args[i], false);
            } else if (args[i+1].equalsIgnoreCase("true!")) {
                root.put(args[i], true);
            } else if (args[i+1].matches("\\d+!")) {
                root.put(args[i], Integer.parseInt(args[i+1].substring(0, args[i+1].length() - 1)));
            } else if (args[i].matches("is.*")) {
                if (args[i+1].matches("true|false")) {
                    root.put(args[i], args[i+1].matches("true") ? true : false);
                }
            } else if (args[i+1].matches("\\d+")) {
                root.put(args[i], Integer.parseInt(args[i+1].substring(0, args[i+1].length())));
            }
        }

        root.put("slashedPackageName", new FmSlashedPackageNameMethod());
        root.put("camelCaseToUnderscore", new FmCamelCaseToUnderscoreMethod());
        root.put("underscoreToCamelCase", new FmUnderscoreToCamelCaseMethod());
        root.put("activityToLayout", new FmActivityToLayoutMethod());
        root.put("layoutToActivity", new FmLayoutToActivityMethod());
        root.put("classToResource", new FmClassNameToResourceMethod());
        root.put("escapeXmlAttribute", new FmEscapeXmlStringMethod());
        root.put("escapeXmlText", new FmEscapeXmlStringMethod());
        root.put("escapeXmlString", new FmEscapeXmlStringMethod());
        root.put("extractLetters", new FmExtractLettersMethod());

        /* Get the template */
        Template temp = cfg.getTemplate(args[1]);

        /* Merge data-model with template */
        Writer out = new OutputStreamWriter(System.out);
        temp.process(root, out);
    }
}
