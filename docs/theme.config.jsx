export default {
  logo: (
    <h1
      style={{
        fontFamily: "monospace",
        padding: "1px 6px",
        borderRadius: "4px",
      }}
    >
      director.el
    </h1>
  ),
  project: {
    link: "https://github.com/bard/emacs-director",
  },
  docsRepositoryBase: "https://github.com/bard/emacs-director/tree/master/docs",
  editLink: {
    text: "Edit this page →",
  },
  feedback: {
    content: "Questions? Send feedback →",
    labels: "feedback",
  },
  toc: {
    float: true,
  },
  useNextSeoProps() {
    return {
      titleTemplate: "%s – emacs-director",
    };
  },
  gitTimestamp: null,
  head: (
    <>
      <meta name="msapplication-TileColor" content="#fff" />
      <meta httpEquiv="Content-Language" content="en" />
      <meta
        name="description"
        content="Script Emacs sessions for automated screencasts and end-to-end tests"
      />
      <meta name="twitter:card" content="summary_large_image" />
      <meta property="og:title" content="emacs-director" />
      <meta name="apple-mobile-web-app-title" content="emacs-director" />
      <meta property="og:title" content="emacs-director" />
      <meta
        property="og:description"
        content="Script Emacs sessions for automated screencasts and end-to-end tests"
      />
    </>
  ),
  footer: {
    text: (
      <span>
        © 2020-{new Date().getFullYear()}{" "}
        <a
          style={{ textDecoration: "underline" }}
          href="https://massimilianomirra.com/"
          target="_blank"
        >
          Massimiliano Mirra
        </a>
      </span>
    ),
  },
};
