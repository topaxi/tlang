export async function compressSource(source: string) {
  let { compressToEncodedURIComponent } = await import('lz-string');

  return compressToEncodedURIComponent(source);
}

export async function decompressSource(source: string) {
  let { decompressFromEncodedURIComponent } = await import('lz-string');

  return decompressFromEncodedURIComponent(source);
}
