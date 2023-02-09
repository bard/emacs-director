import remarkSmartypants from "remark-smartypants";
import nextra from "nextra";

const withNextra = nextra({
  theme: "nextra-theme-docs",
  themeConfig: "./theme.config.jsx",
  mdxOptions: {
    remarkPlugins: [remarkSmartypants],
  },
});

let assetPrefix, basePath;
if (process.env.GITHUB_ACTIONS) {
  const repo = process.env.GITHUB_REPOSITORY.replace(/.*?\//, "");
  assetPrefix = `/${repo}/`;
  basePath = `/${repo}`;
} else {
  assetPrefix = undefined;
  basePath = "";
}

export default withNextra({
  assetPrefix,
  basePath,
  images: {
    unoptimized: true,
  },
});
