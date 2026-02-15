// @ts-check

import {themes as prismThemes} from 'prism-react-renderer';

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'NordKredit Knowledge Base',
  tagline: 'Core Banking Migration — Business Rules & Regulatory Traceability',
  favicon: 'img/favicon.ico',

  future: {
    v4: true,
  },

  url: 'https://agentic-delivery.github.io',
  baseUrl: '/nordkredit-core-banking/',

  organizationName: 'Agentic-Delivery',
  projectName: 'nordkredit-core-banking',

  onBrokenLinks: 'throw',

  markdown: {
    hooks: {
      onBrokenMarkdownLinks: 'warn',
    },
  },

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: './sidebars.js',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      colorMode: {
        respectPrefersColorScheme: true,
      },
      navbar: {
        title: 'NordKredit',
        items: [
          {
            type: 'docSidebar',
            sidebarId: 'mainSidebar',
            position: 'left',
            label: 'Documentation',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Documentation',
            items: [
              {
                label: 'Business Rules',
                to: '/docs/business-rules/account-management/',
              },
              {
                label: 'Data Structures',
                to: '/docs/data-structures/',
              },
            ],
          },
          {
            title: 'Compliance',
            items: [
              {
                label: 'Regulatory Traceability',
                to: '/docs/regulatory-traceability/',
              },
              {
                label: 'Batch Processes',
                to: '/docs/batch-processes/',
              },
            ],
          },
        ],
        copyright: `Copyright \u00a9 ${new Date().getFullYear()} NordKredit AB. Internal use only.`,
      },
      prism: {
        theme: prismThemes.github,
        darkTheme: prismThemes.dracula,
        additionalLanguages: ['cobol', 'csharp', 'gherkin'],
      },
    }),
};

export default config;
