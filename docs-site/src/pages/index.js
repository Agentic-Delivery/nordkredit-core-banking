import clsx from 'clsx';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Heading from '@theme/Heading';
import styles from './index.module.css';

const sections = [
  {
    title: 'Business Rules',
    description:
      'Extracted business logic from COBOL source programs, organized by domain: Account Management, Card Management, Transactions, User/Security, Billing, and Reporting.',
    link: '/docs/business-rules/account-management/',
  },
  {
    title: 'Data Structures',
    description:
      'COBOL copybook definitions mapped to .NET data models. Includes field-level mappings, EBCDIC-to-Unicode conversion notes, and validation rules.',
    link: '/docs/data-structures/',
  },
  {
    title: 'Batch Processes',
    description:
      'JCL batch job documentation with Azure Functions migration mappings. Covers nightly interest calculation, monthly statements, regulatory reporting, and AML screening.',
    link: '/docs/batch-processes/',
  },
  {
    title: 'Regulatory Traceability',
    description:
      'Traceability matrices linking every business rule to its applicable regulation (FSA, PSD2, GDPR, AML/KYC, DORA) and originating COBOL program.',
    link: '/docs/regulatory-traceability/',
  },
];

function Section({title, description, link}) {
  return (
    <div className={clsx('col col--6')}>
      <div className={styles.section}>
        <Heading as="h3">{title}</Heading>
        <p>{description}</p>
        <Link className="button button--secondary button--sm" to={link}>
          Browse {title}
        </Link>
      </div>
    </div>
  );
}

function HomepageHeader() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <header className={clsx('hero hero--primary', styles.heroBanner)}>
      <div className="container">
        <Heading as="h1" className="hero__title">
          {siteConfig.title}
        </Heading>
        <p className="hero__subtitle">{siteConfig.tagline}</p>
        <div className={styles.buttons}>
          <Link
            className="button button--secondary button--lg"
            to="/docs/business-rules/account-management/">
            Get Started
          </Link>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  const {siteConfig} = useDocusaurusContext();
  return (
    <Layout
      title="Home"
      description="NordKredit AB core banking migration knowledge base — extracted COBOL business rules, data structures, batch processes, and regulatory traceability.">
      <HomepageHeader />
      <main>
        <section className={styles.sections}>
          <div className="container">
            <Heading as="h2" className={styles.sectionsHeading}>
              What is this site?
            </Heading>
            <p className={styles.sectionsDescription}>
              This knowledge base captures the business rules, data structures, and regulatory
              mappings extracted from NordKredit AB&apos;s IBM z/OS mainframe system during the
              core banking migration to .NET 8 on Azure. It serves as the single source of truth
              for the migration team, domain experts, and regulatory auditors.
            </p>
            <Heading as="h3" className={styles.sectionsSubheading}>
              Target Audience
            </Heading>
            <ul>
              <li><strong>Migration developers</strong> — reference extracted business rules when implementing .NET equivalents</li>
              <li><strong>Domain experts (COBOL developers)</strong> — validate extracted rules against mainframe source</li>
              <li><strong>Regulatory &amp; compliance officers</strong> — verify traceability from regulation to implementation</li>
              <li><strong>Project management</strong> — track extraction progress across domains</li>
            </ul>
            <Heading as="h3" className={styles.sectionsSubheading}>
              How to Navigate
            </Heading>
            <p>
              Use the sidebar to browse by section. Business rules are organized by domain.
              Each rule links back to its COBOL source program and applicable regulations.
            </p>
            <div className="row">
              {sections.map((props, idx) => (
                <Section key={idx} {...props} />
              ))}
            </div>
          </div>
        </section>
      </main>
    </Layout>
  );
}
